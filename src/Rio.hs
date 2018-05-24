{-# LANGUAGE NoImplicitPrelude #-}

module Rio where

import ClassyPrelude
import Control.Monad.Fail


newtype Rio e a = Rio { unRio :: ReaderT e IO a }
  deriving newtype (MonadReader e, MonadIO, Functor, Applicative, Monad
    , MonadBase IO, MonadFail, MonadThrow)


instance MonadBaseControl IO (Rio e) where
  type StM (Rio e) a = a
  liftBaseWith f = Rio $ liftBaseWith $ \q -> f (q . unRio)
  restoreM = Rio . restoreM

