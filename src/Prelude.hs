module Prelude
  ( module Relude
  , module Control.Lens
  , module Control.Exception.Safe
  , module Data.Generics.Product.Fields
  , Rio
  , MonadFail(..)
  , Vector
  , unwrap
  ) where


import Relude hiding ((??), uncons)
import Control.Exception.Safe hiding (Handler)
import Control.Lens hiding (Context)
import Data.Generics.Product.Fields
import Rio (Rio)
import Data.Vector (Vector)
import GHC.OverloadedLabels

unwrap :: (Wrapped a, Unwrapped a ~ b) => a -> b
unwrap = view _Wrapped'

instance HasField' field a b => IsLabel field (a -> b) where
  fromLabel = getField @field
