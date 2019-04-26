{-# LANGUAGE NoImplicitPrelude #-}

module Rio where

import Relude
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch


newtype Rio e a = Rio { unRio :: ReaderT e IO a }
  deriving newtype (MonadReader e, MonadIO, Functor, Applicative, Monad
                   , MonadBase IO, MonadFail, MonadThrow, MonadCatch)

runRio :: e -> Rio e a -> IO a
runRio e rio = runReaderT (unRio rio) e

instance MonadBaseControl IO (Rio e) where
  type StM (Rio e) a = a
  liftBaseWith f = Rio $ liftBaseWith $ \q -> f (q . unRio)
  restoreM = Rio . restoreM

contraMapRio :: (e -> e') -> Rio e' a -> Rio e a
contraMapRio f = Rio . withReaderT f . unRio
