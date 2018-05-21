module Jaeger.SpanContext where


spanToSpanContext :: Span -> SpanContext
spanToSpanContext Span{spanId = sctxSpanId, spanTraceId = sctxTraceId} =
  SpanContext {..}


data SpanContext = SpanContext
  { sctxTraceId :: !Int64
  , sctxSpanId  :: !Int64
  }
  deriving (Eq, Show, Generic, Serialise)
