{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jaeger.Tracer where

import qualified Agent                          as Thrift
import           Control.Concurrent             (ThreadId, myThreadId)
import           Control.Exception
import           Data.Foldable                  (for_)
import           Data.Functor                   (void)
import           Data.Int
import           Data.IORef
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text.Lazy                 as LT
import           Data.Time.Clock.POSIX
import qualified Data.Vector                    as V
import           Jaeger.Types
import qualified Jaeger_Types                   as Thrift
import           Network.HTTP.Types.Header      (HeaderName)
import           Network.Socket                 hiding (send)
import           Network.Socket.ByteString.Lazy
import           System.Clock
import           System.Random
import qualified Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import qualified Thrift.Transport.IOBuffer      as Thrift
import qualified Thrift.Types                   as Thrift

data Tracer = Tracer
  { tracerSocket        :: !Socket
  , tracerWriteBuffer   :: !Thrift.WriteBuffer
  , tracerConfiguration :: !TracerConfiguration
  , tracerActiveSpan    :: !(IORef (Map.Map ThreadId Span))
  , tracerIdGenerator   :: !(IO Int64)
  }



instance Thrift.Transport Tracer where
  tFlush t =
    do
      bytes <-
        Thrift.flushBuf (tracerWriteBuffer t)

      void $ send (tracerSocket t) bytes

      return ()

  tWrite =
    Thrift.writeBuf . tracerWriteBuffer


data TracerConfiguration = TracerConfiguration
  { tracerHostName    :: !String
  , tracerPort        :: !String
  , tracerServiceName :: !Text
  }


openTracer :: TracerConfiguration -> IO Tracer
openTracer tracerConfiguration@TracerConfiguration{tracerHostName, tracerPort} =
  do
    addr : _ <-
      getAddrInfo
        (Just defaultHints { addrSocketType = Datagram })
        (Just tracerHostName)
        (Just tracerPort)

    tracerSocket <-
      socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect tracerSocket (addrAddress addr)

    tracerWriteBuffer <-
      Thrift.newWriteBuffer

    tracerActiveSpan <-
      newIORef mempty

    return Tracer { tracerIdGenerator = randomIO, ..}


inSpan :: Tracer -> Text -> Maybe TraceId -> IO a -> IO a
inSpan tracer spanOperationName traceIdM io =
  do
    bracket_
      (pushSpan tracer spanOperationName traceIdM)
      (popSpan tracer)
      io

  where

popSpan :: Tracer -> IO ()
popSpan tracer =
  do
    activeSpan <- readActiveSpan tracer

    maybe (clearActiveSpan tracer) (writeActiveSpan tracer) (spanParent =<< activeSpan)

    completed <-
      getTime Monotonic

    for_ activeSpan $ \sp  ->
      reportSpan tracer sp
        { spanDuration =
            Just
            (toMicroSeconds
              (diffTimeSpec completed (spanMonotonicTime sp)))
        }   `catch` \ (_ :: IOException) -> pure ()


pushSpan :: Tracer -> Text -> Maybe TraceId -> IO ()
pushSpan tracer@Tracer{tracerIdGenerator}  spanOperationName  traceIdM =
  do
    spanOpenedAt <-
      getPOSIXTime

    spanMonotonicTime <-
      getTime Monotonic

    spanId <-
      tracerIdGenerator

    spanParent <-
      readActiveSpan tracer

    spanTraceId <-
      case traceIdM of
        Just traceId -> pure traceId
        Nothing ->
          maybe tracerIdGenerator pure (fmap spanTraceId spanParent)

    let
      newSpan =
        Span { spanDuration = Nothing
             , spanTags = mempty
             , spanReferences = []
             , spanBaggage = mempty
             , ..
             }

    writeActiveSpan tracer newSpan


clearActiveSpan :: Tracer -> IO ()
clearActiveSpan Tracer{tracerActiveSpan} = do
  tid <- myThreadId
  atomicModifyIORef tracerActiveSpan (\ m -> (Map.delete tid m, ()))

writeActiveSpan :: Tracer -> Span -> IO ()
writeActiveSpan Tracer{tracerActiveSpan} activeSpan = do
  tid <- myThreadId
  atomicModifyIORef tracerActiveSpan (\ m -> (Map.insert tid activeSpan m, ()))

readActiveSpan :: Tracer -> IO (Maybe Span)
readActiveSpan Tracer{tracerActiveSpan} = do
  active <- readIORef tracerActiveSpan
  tid <- myThreadId
  pure $ Map.lookup tid active

tagActiveSpan :: Tracer -> Text -> TagValue -> IO ()
tagActiveSpan Tracer{tracerActiveSpan} k v =
  modifyIORef tracerActiveSpan $ \s ->
    flip fmap s $ \sp ->
      sp { spanTags = Map.insert k v (spanTags sp) }


reportSpan :: Tracer -> Span -> IO ()
reportSpan tracer span =
  do
    let
      proto =
        Thrift.CompactProtocol tracer

      Tracer{tracerConfiguration} =
        tracer

      TracerConfiguration{tracerServiceName} =
        tracerConfiguration

    Thrift.writeMessage proto ("emitBatch", Thrift.M_ONEWAY, 0) $
      Thrift.write_EmitBatch_args proto
      (Thrift.EmitBatch_args
        (Thrift.Batch
          (Thrift.Process (LT.fromStrict tracerServiceName) Nothing)
          (pure Thrift.Span
              { Thrift.span_traceIdLow =
                  spanTraceId span
              , Thrift.span_traceIdHigh =
                  0
              , Thrift.span_spanId =
                  spanId span
              , Thrift.span_parentSpanId =
                  fromMaybe 0 (spanId <$> spanParent span)
              , Thrift.span_operationName =
                  LT.fromStrict (spanOperationName span)
              , Thrift.span_references =
                  case spanReferences span of
                    [ ] ->
                      Nothing

                    follows ->
                      Just $ V.fromList $
                      map
                        (\ref ->
                           let toSpanRef refType ctx = Thrift.SpanRef {
                                              Thrift.spanRef_refType =
                                                refType
                                            , Thrift.spanRef_traceIdLow =
                                                sctxTraceId ctx
                                            , Thrift.spanRef_traceIdHigh =
                                                0
                                            , Thrift.spanRef_spanId =
                                                sctxSpanId ctx
                                            }
                           in
                           case ref of
                             ChildOf ctx -> toSpanRef Thrift.CHILD_OF ctx
                             FollowsFrom ctx -> toSpanRef Thrift.FOLLOWS_FROM ctx)
                        follows
              , Thrift.span_flags =
                  0
              , Thrift.span_startTime = startTime
              , Thrift.span_duration =
                  fromMaybe 0 (spanDuration span)
              , Thrift.span_tags =
                  case Map.toList (spanTags span) of
                    [ ] ->
                      Nothing

                    tags ->
                      Just (V.fromList (map (uncurry makeTag) tags))
              , Thrift.span_logs =
                  case Map.toList (spanBaggage span) of
                    [] ->
                      Nothing

                    baggage ->
                      Just
                        (V.fromList
                           (map
                              (\(k, v) ->
                                 Thrift.Log { log_timestamp = startTime
                                            , log_fields =
                                                V.fromList [ makeTag "event" (StringTag "baggage")
                                                           , makeTag "key" (StringTag k)
                                                           , makeTag "value" (StringTag v)
                                                           ]
                                            })
                              baggage))
              })))

    Thrift.tFlush tracer

  where

    startTime =
      round (spanOpenedAt span / 0.000001)

    makeTag k v =
      Thrift.Tag
        { tag_key = LT.fromStrict k
        , tag_vType =
            case v of
              BoolTag{}   -> Thrift.BOOL
              StringTag{} -> Thrift.STRING
              DoubleTag{} -> Thrift.DOUBLE
              IntTag{}    -> Thrift.LONG
              BinaryTag{} -> Thrift.BINARY
        , tag_vStr =
            case v of
              StringTag t -> Just (LT.fromStrict t)
              _           -> Nothing
        , tag_vDouble =
            case v of
              DoubleTag a -> Just a
              _           -> Nothing
        , tag_vBool =
            case v of
              BoolTag a -> Just a
              _         -> Nothing
        , tag_vLong =
            case v of
              IntTag a -> Just a
              _        -> Nothing
        , tag_vBinary =
            case v of
              BinaryTag a -> Just a
              _           -> Nothing
        }


toMicroSeconds :: TimeSpec -> Int64
toMicroSeconds ts =
  round (fromIntegral (toNanoSecs ts) / (1000 :: Double))



activeSpanFollowsFrom :: Tracer -> SpanContext -> IO ()
activeSpanFollowsFrom Tracer{tracerActiveSpan} ctx =
  modifyIORef tracerActiveSpan $
  fmap $ \sp ->
    sp { spanReferences = FollowsFrom ctx : spanReferences sp }

activeSpanIsAChildOf :: Tracer -> SpanContext -> IO ()
activeSpanIsAChildOf Tracer{tracerActiveSpan} ctx =
  modifyIORef tracerActiveSpan $
  fmap $ \sp ->
    sp { spanReferences = ChildOf ctx : spanReferences sp }

setActiveSpanBaggage :: Tracer -> Text -> Text -> IO ()
setActiveSpanBaggage Tracer{tracerActiveSpan} k v =
  modifyIORef tracerActiveSpan $
  fmap $ \sp ->
    sp { spanBaggage = Map.insert k v (spanBaggage sp) }
