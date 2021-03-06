{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableSuperClasses #-}
{-# language UndecidableInstances #-}

module Control.Monad.OpenTracing where

import Data.Text
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import OpenTracing

class MonadUnliftIO m => MonadTracing m where
  askTracer :: m Tracer

newtype TracingT m a = TracingT { unTracingT :: ReaderT Tracer m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (TracingT m) where
  askUnliftIO = TracingT $ ReaderT $ \env ->
    do UnliftIO unlift <- askUnliftIO
       return (UnliftIO (\(TracingT (ReaderT f)) -> unlift (f env)))

instance MonadUnliftIO m => MonadTracing (TracingT m) where
  askTracer = TracingT ask

runTracingT :: TracingT m a -> Tracer -> m a
runTracingT (TracingT m) = runReaderT m

inSpan :: (MonadTracing m) => Text -> m a -> m a
inSpan operationName action =
  do
    UnliftIO unlift <- askUnliftIO
    t <- askTracer
    liftIO (OpenTracing.inSpan t operationName (unlift action))
