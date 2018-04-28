{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.OpenTracing
import           Jaeger
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.OpenTracing
import qualified TestLib
import           Web.Scotty

main :: IO ()
main = do
  t <-
    openTracer TracerConfiguration { tracerServiceName = "test-exe" }

--  runTracingT TestLib.hello t

  app <- scottyApp $
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

  run 3000 (openTracingMiddleware t app)
