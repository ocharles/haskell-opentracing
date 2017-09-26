{-# language OverloadedStrings #-}

module TestLib where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.OpenTracing

hello :: (MonadIO m, MonadTracing m) => m ()
hello =
  inSpan "hello" (liftIO $ threadDelay 10000 >> putStrLn "Hello!")
