{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jaeger where

import qualified Agent                          as Thrift
import           Codec.Serialise
import qualified Collector                      as Thrift
import           Control.Concurrent             (ThreadId, myThreadId)
import           Control.Exception
import           Control.Monad
import           Control.Monad                  (msum)
import           Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy           as LBS
import           Data.Foldable                  (for_)
import           Data.Int
import           Data.IORef
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                      (Text)
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                 as LT
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Vector                    as V
import           GHC.Generics                   (Generic)
import qualified Jaeger_Types                   as Thrift
import           Network.HTTP.Types.Header      (Header, HeaderName)
import           Network.Socket                 hiding (send)
import           Network.Socket.ByteString.Lazy
import           System.Clock
import           System.Random
import qualified Thrift
import qualified Thrift.Protocol                as Thrift
import qualified Thrift.Protocol.Binary         as Thrift
import qualified Thrift.Protocol.Compact        as Thrift
import qualified Thrift.Transport               as Thrift
import qualified Thrift.Transport.IOBuffer      as Thrift
import qualified Thrift.Types                   as Thrift

data Reference = ChildOf !SpanContext | FollowsFrom !SpanContext

type TraceId = Int64

data Span = Span
  { spanOperationName :: !Text
  , spanOpenedAt      :: !POSIXTime
  , spanMonotonicTime :: !TimeSpec
  , spanId            :: !TraceId
  , spanParent        :: !(Maybe Span)
  , spanTraceId       :: !TraceId
  , spanDuration      :: !(Maybe Int64)
  , spanTags          :: !(Map.Map Text TagValue)
  , spanReferences    :: ![Reference]
  , spanBaggage       :: !(Map.Map Text Text)
  }


data TagValue
  = BoolTag !Bool
  | StringTag !Text
  | DoubleTag !Double
  | IntTag !Int64
  | BinaryTag !LBS.ByteString

intTag = IntTag
boolTag = BoolTag
stringTag = StringTag

data Tracer = Tracer
  { tracerSocket        :: !Socket
  , tracerWriteBuffer   :: !Thrift.WriteBuffer
  , tracerConfiguration :: !TracerConfiguration
  , tracerActiveSpan    :: !(IORef (Map.Map ThreadId Span))
  , tracerIdGenerator   :: !(IO Int64)
  }


tracingHeader :: HeaderName
tracingHeader = "uber-tracing-id"


instance Thrift.Transport Tracer where
  tFlush t =
    do
      bytes <-
        Thrift.flushBuf (tracerWriteBuffer t)

      send (tracerSocket t) bytes

      return ()

  tWrite =
    Thrift.writeBuf . tracerWriteBuffer


data TracerConfiguration = TracerConfiguration
  { tracerHostName    :: !String
  , tracerPort        :: !String
  , tracerServiceName :: !Text
  }


openTracer :: TracerConfiguration -> IO Tracer
openTracer tracerConfiguration@TracerConfiguration{tracerHostName, tracerPort, tracerServiceName} =
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
inSpan tracer@Tracer{tracerActiveSpan, tracerIdGenerator} spanOperationName traceIdM io =
  do
    spanParent <-
      readIORef tracerActiveSpan

    bracket_
      prepareSpan
      (finish spanParent)
      io

  where

    prepareSpan =
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
          span =
            Span { spanDuration = Nothing
                 , spanTags = mempty
                 , spanReferences = []
                 , spanBaggage = mempty
                 , ..
                 }

        writeActiveSpan tracer span

    finish spanParent =
      do
        span <-
          readIORef tracerActiveSpan

        writeIORef tracerActiveSpan spanParent

        completed <-
          getTime Monotonic

        for_ span $ \span ->
          reportSpan tracer span
            { spanDuration =
                Just
                (toMicroSeconds
                  (diffTimeSpec completed (spanMonotonicTime span)))
            }   `catch` \ (_ :: IOException) -> pure ()

writeActiveSpan :: Tracer -> Span -> IO ()
writeActiveSpan Tracer{tracerActiveSpan} span = do
  tid <- myThreadId
  atomicModifyIORef tracerActiveSpan (\ m -> (Map.insert tid span m, ()))

readActiveSpan :: Tracer -> IO (Maybe Span)
readActiveSpan Tracer{tracerActiveSpan} = do
  active <- readIORef tracerActiveSpan
  tid <- myThreadId
  pure $ Map.lookup tid active

tagActiveSpan :: Tracer -> Text -> TagValue -> IO ()
tagActiveSpan Tracer{tracerActiveSpan} k v =
  modifyIORef tracerActiveSpan $ \s ->
    flip fmap s $ \s ->
      s { spanTags = Map.insert k v (spanTags s) }


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


class Carrier a where
  inject :: Tracer -> Span -> a -> a
  extract :: Tracer -> a -> Maybe SpanContext


instance Carrier [Header] where
  extract _ =
    msum .
    map (either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict . snd) .
    filter (\(n, _) -> n == spanHeader)

  inject _ span headers =
    (spanHeader, LBS.toStrict (serialise (spanToSpanContext span)))
      : filter (\(n, _) -> n /= spanHeader) headers

instance Carrier (Map.Map Text Text) where
  extract _ =
    hush . deserialiseOrFail . LBS.fromStrict <=<
    hush. B64.decode . encodeUtf8 <=<
    Map.lookup "Span"
    where hush = either (const Nothing) Just

  inject _ span =
    Map.insert "Span"
      (decodeUtf8 (B64.encode (LBS.toStrict (serialise (spanToSpanContext span)))))


spanHeader :: HeaderName
spanHeader =
  "X-OpenTracing-Span"


spanToSpanContext :: Span -> SpanContext
spanToSpanContext Span{spanId = sctxSpanId, spanTraceId = sctxTraceId} =
  SpanContext {..}


data SpanContext = SpanContext
  { sctxTraceId :: !Int64
  , sctxSpanId  :: !Int64
  }
  deriving (Generic, Serialise)


activeSpanFollowsFrom :: Tracer -> SpanContext -> IO ()
activeSpanFollowsFrom Tracer{tracerActiveSpan} ctx =
  modifyIORef tracerActiveSpan $
  fmap $ \span ->
    span { spanReferences = FollowsFrom ctx : spanReferences span }

activeSpanIsAChildOf :: Tracer -> SpanContext -> IO ()
activeSpanIsAChildOf Tracer{tracerActiveSpan} ctx =
  modifyIORef tracerActiveSpan $
  fmap $ \span ->
    span { spanReferences = ChildOf ctx : spanReferences span }

setActiveSpanBaggage :: Tracer -> Text -> Text -> IO ()
setActiveSpanBaggage Tracer{tracerActiveSpan} k v =
  modifyIORef tracerActiveSpan $
  fmap $ \span ->
    span { spanBaggage = Map.insert k v (spanBaggage span) }
