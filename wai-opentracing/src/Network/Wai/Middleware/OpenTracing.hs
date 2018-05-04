{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.OpenTracing where

import           Data.IORef
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy     as LT
import           Jaeger
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
  inSpan tracer (T.intercalate "/" (pathInfo req)) $
  (do
      activeSpanM <- readIORef tracerActiveSpan
      case activeSpanM of
        Nothing -> app req onResponse
        Just activeSpan ->
          app req{requestHeaders = (tracingHeader, encodeUtf8 $ T.pack $ show $ spanId activeSpan) : (requestHeaders req)} onResponse
  )
