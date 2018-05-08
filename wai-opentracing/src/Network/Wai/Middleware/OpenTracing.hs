{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.OpenTracing where

import           Data.ByteString           (ByteString)
import           Data.IORef
import           Data.List                 (find)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy            as LT
import           Data.Text.Read
import           Jaeger
import           Network.HTTP.Types.Header
import           Network.Wai

-- | Manages <http://opentracing.io/ OpenTracing> spans over a WAI application
--
-- This middleware will emit a /span/ identified by the request's `pathInfo` for
-- each query received by the wrapped `Application`.
--
--  * If a `uber-tracing-id` header is set it will be used as a /parent/ for the whole request's span
--  * If it is not set then an orphan span will be emitted
--
-- In all cases this middleware will also /set/ the @uber-tracing-id@ header to the new span's
-- identifier so that downstream spans can be correctly linked to the whole request.
openTracingMiddleware :: Tracer -> Middleware
openTracingMiddleware tracer@(Tracer {tracerActiveSpan}) app = \req onResponse ->
  let parentIdM = extract tracer $ requestHeaders req in
  inSpan tracer (T.intercalate "/" (pathInfo req)) (sctxTraceId <$> parentIdM) $ do
    maybe (pure ()) (activeSpanIsAChildOf tracer) parentIdM
    activeSpanM <- readActiveSpan tracer
    case activeSpanM of
      Nothing -> app req onResponse
      Just activeSpan ->
        app req {requestHeaders = inject tracer activeSpan (requestHeaders req)} onResponse
