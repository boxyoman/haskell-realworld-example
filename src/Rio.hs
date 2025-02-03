{-# LANGUAGE NoImplicitPrelude #-}

module Rio where

import Relude (
  Applicative,
  Functor,
  IO,
  Monad,
  MonadFail,
  MonadIO,
  MonadReader,
  ReaderT (..),
  withReaderT,
  (.),
 )
import UnliftIO (MonadUnliftIO)


newtype Rio e a = Rio { unRio :: ReaderT e IO a }
  deriving newtype (MonadReader e, MonadIO, Functor, Applicative, Monad, MonadFail, MonadUnliftIO)

runRio :: e -> Rio e a -> IO a
runRio e rio = runReaderT (unRio rio) e

contraMapRio :: (e -> e') -> Rio e' a -> Rio e a
contraMapRio f = Rio . withReaderT f . unRio


