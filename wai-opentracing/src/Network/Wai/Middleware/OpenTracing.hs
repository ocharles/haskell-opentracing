{-# language OverloadedStrings #-}

module Network.Wai.Middleware.OpenTracing where

import qualified Data.Text as T
import Network.Wai
import OpenTracing

openTracingMiddleware :: Tracer -> Middleware
openTracingMiddleware tracer app = \req onResponse ->
  inSpan tracer (T.intercalate "/" (pathInfo req)) (app req onResponse)
