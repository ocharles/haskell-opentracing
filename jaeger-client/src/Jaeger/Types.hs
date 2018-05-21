{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Jaeger.Types where

import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX
import           GHC.Generics          (Generic)
import           System.Clock

data Reference = ChildOf !SpanContext | FollowsFrom !SpanContext
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data TagValue
  = BoolTag !Bool
  | StringTag !Text
  | DoubleTag !Double
  | IntTag !Int64
  | BinaryTag !LBS.ByteString
  deriving (Eq, Show)

intTag :: Int64 -> TagValue
intTag = IntTag

boolTag :: Bool -> TagValue
boolTag = BoolTag

stringTag :: Text -> TagValue
stringTag = StringTag

spanToSpanContext :: Span -> SpanContext
spanToSpanContext Span{spanId = sctxSpanId, spanTraceId = sctxTraceId} =
  SpanContext {..}

data SpanContext = SpanContext
  { sctxTraceId :: !Int64
  , sctxSpanId  :: !Int64
  }
  deriving (Eq, Show, Generic, Serialise)
