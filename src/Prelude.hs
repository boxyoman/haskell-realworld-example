module Prelude
  ( module ClassyPrelude
  , module Control.Lens
  , module Data.Generics.Product.Fields
  , Type
  , Rio
  , MonadFail(..)
  ) where


import ClassyPrelude hiding
  (Index, (<.>), uncons, (<|), index, unsnoc, cons, snoc, fail, delete)
import Control.Monad.Fail
import Control.Lens
import Data.Generics.Product.Fields
import Data.Kind (Type)
import Rio (Rio)
import GHC.OverloadedLabels


instance HasField' field a b => IsLabel field (a -> b) where
  fromLabel = getField @field
