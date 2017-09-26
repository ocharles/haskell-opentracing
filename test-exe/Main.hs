{-# language OverloadedStrings #-}

module Main where

import Control.Monad.OpenTracing
import Jaeger
import qualified TestLib

main :: IO ()
main = do
  t <-
    openTracer TracerConfiguration { tracerServiceName = "test-exe" }

  runTracingT TestLib.hello t

  putStrLn "Done"
