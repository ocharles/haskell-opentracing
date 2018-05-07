{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Format of Jaeger's distributed traces ids to be transmitted across network -}
module Jaeger.Carrier where

import           Codec.Serialise
import           Control.Monad               (msum, (<=<))
import qualified Data.ByteString.Base16.Lazy as Hex
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Lazy        as LBS
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict             as Map
import           Data.Monoid
import qualified Data.Serialize              as Ser
import           Data.Text                   (Text)
import           Data.Text.Encoding
import           Jaeger.Tracer
import           Jaeger.Types
import           Network.HTTP.Types.Header   (Header, HeaderName)

class Carrier a where
  inject :: Tracer -> Span -> a -> a
  extract :: Tracer -> a -> Maybe SpanContext

tracingHeader :: HeaderName
tracingHeader = "uber-tracing-id"

spanHeader :: HeaderName
spanHeader =
  "X-OpenTracing-Span"

decodeSpanContext :: LBS.ByteString -> Maybe SpanContext
decodeSpanContext bs =
  case LBS.split (fromIntegral $ fromEnum ':') bs of
    [tid,spid,_,bits] -> SpanContext <$> toInt64 tid <*> toInt64 spid
    _                 -> Nothing

encodeSpanContext :: SpanContext -> LBS.ByteString
encodeSpanContext (SpanContext tid spid) = fromInt64 tid <> ":" <> fromInt64 spid <> ":00:00"

toInt64 :: LBS.ByteString -> Maybe Int64
toInt64 = either (const Nothing) Just . Ser.decode . LBS.toStrict . fst . Hex.decode

fromInt64 :: Int64 -> LBS.ByteString
fromInt64 = Hex.encode . LBS.fromStrict . Ser.encode

instance Carrier [Header] where
  extract _ =
    msum .
    map (decodeSpanContext . LBS.fromStrict . snd) .
    filter (\(n, _) -> n == tracingHeader)

  inject _ sp headers =
    (tracingHeader, LBS.toStrict . encodeSpanContext $ spanToSpanContext sp)
      : filter (\(n, _) -> n /= spanHeader) headers

instance Carrier (Map.Map Text Text) where
  extract _ =
    hush . deserialiseOrFail . LBS.fromStrict <=<
    hush. B64.decode . encodeUtf8 <=<
    Map.lookup "Span"
    where hush = either (const Nothing) Just

  inject _ sp =
    Map.insert "Span"
      (decodeUtf8 . B64.encode . LBS.toStrict . serialise . spanToSpanContext $ sp)
