{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Jaeger where

import GHC.Generics (Generic)
import Control.Monad (msum)
import qualified Agent as Thrift
import Codec.Serialise
import qualified Collector as Thrift
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import qualified Jaeger_Types as Thrift
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import System.Clock
import System.Random
import qualified Thrift
import qualified Thrift.Protocol as Thrift
import qualified Thrift.Protocol.Binary as Thrift
import qualified Thrift.Protocol.Compact as Thrift
import qualified Thrift.Transport as Thrift
import qualified Thrift.Transport.IOBuffer as Thrift
import qualified Thrift.Types as Thrift

data Span = Span
  { spanOperationName :: !Text
  , spanOpenedAt :: !POSIXTime
  , spanMonotonicTime :: !TimeSpec
  , spanId :: !Int64
  , spanParent :: !(Maybe Span)
  , spanTraceId :: !Int64
  , spanDuration :: !(Maybe Int64)
  , spanTags :: !(Map.Map Text TagValue)
  , spanFollowsFrom :: ![SpanContext]
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
  { tracerSocket :: !Socket
  , tracerWriteBuffer :: !Thrift.WriteBuffer
  , tracerConfiguration :: !TracerConfiguration
  , tracerActiveSpan :: !(IORef (Maybe Span))
  }


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
  { tracerServiceName :: !Text
  }


openTracer :: TracerConfiguration -> IO Tracer
openTracer tracerConfiguration =
  do
    addr : _ <-
      getAddrInfo
        (Just defaultHints { addrSocketType = Datagram })
        (Just "127.0.0.1")
        (Just "6831")

    tracerSocket <-
      socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

    connect tracerSocket (addrAddress addr)

    tracerWriteBuffer <-
      Thrift.newWriteBuffer

    tracerActiveSpan <-
      newIORef Nothing

    return Tracer {..}


inSpan :: Tracer -> Text -> IO a -> IO a
inSpan tracer@Tracer{tracerActiveSpan} spanOperationName io =
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
          randomIO

        spanParent <-
          readIORef tracerActiveSpan

        spanTraceId <-
          maybe randomIO return (fmap spanTraceId spanParent)

        let
          span =
            Span { spanDuration = Nothing
                 , spanTags = mempty
                 , spanFollowsFrom = []
                 , ..
                 }

        writeIORef tracerActiveSpan (Just span)

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
            }


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

    Thrift.writeMessageBegin proto ("emitBatch", Thrift.M_ONEWAY, 0)

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
                  case spanFollowsFrom span of
                    [ ] ->
                      Nothing

                    follows ->
                      Just $ V.fromList $
                      map
                        (\SpanContext {sctxSpanId, sctxTraceId } ->
                           Thrift.SpanRef { Thrift.spanRef_refType =
                                              Thrift.FOLLOWS_FROM
                                          , Thrift.spanRef_traceIdLow =
                                              sctxTraceId
                                          , Thrift.spanRef_traceIdHigh =
                                              0
                                          , Thrift.spanRef_spanId =
                                              sctxSpanId
                                          })
                        follows
              , Thrift.span_flags =
                  0
              , Thrift.span_startTime =
                  round (spanOpenedAt span / 0.000001)
              , Thrift.span_duration =
                  fromMaybe 0 (spanDuration span)
              , Thrift.span_tags =
                  case Map.toList (spanTags span) of
                    [ ] ->
                      Nothing

                    tags ->
                      Just (V.fromList (map (uncurry makeTag) tags))
              , Thrift.span_logs =
                  Nothing
              })))

    Thrift.writeMessageEnd proto

    Thrift.tFlush tracer

  where

    makeTag k v =
      Thrift.Tag
        { tag_key = LT.fromStrict k
        , tag_vType =
            case v of
              BoolTag{} -> Thrift.BOOL
              StringTag{} -> Thrift.STRING
              DoubleTag{} -> Thrift.DOUBLE
              IntTag{} -> Thrift.LONG
              BinaryTag{} -> Thrift.BINARY
        , tag_vStr =
            case v of
              StringTag t -> Just (LT.fromStrict t)
              _ -> Nothing
        , tag_vDouble =
            case v of
              DoubleTag a -> Just a
              _ -> Nothing
        , tag_vBool =
            case v of
              BoolTag a -> Just a
              _ -> Nothing
        , tag_vLong =
            case v of
              IntTag a -> Just a
              _ -> Nothing
        , tag_vBinary =
            case v of
              BinaryTag a -> Just a
              _ -> Nothing
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


spanHeader :: HeaderName
spanHeader =
  "X-OpenTracing-Span"


spanToSpanContext :: Span -> SpanContext
spanToSpanContext Span{spanId = sctxSpanId, spanTraceId = sctxTraceId} =
  SpanContext {..}


data SpanContext = SpanContext
  { sctxTraceId :: !Int64
  , sctxSpanId :: !Int64
  }
  deriving (Generic, Serialise)


activeSpanFollowsFrom :: Tracer -> SpanContext -> IO ()
activeSpanFollowsFrom Tracer{tracerActiveSpan} ctx =
  modifyIORef tracerActiveSpan $
  fmap $ \span ->
    span { spanFollowsFrom = ctx : spanFollowsFrom span }
