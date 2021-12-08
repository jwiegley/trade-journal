{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Journal.Types.Lot where

import Amount
import Control.Lens
import Data.Sum
import Data.Text (Text)
import GHC.Generics hiding (to)
import GHC.TypeLits
import Journal.Split
import Journal.SumLens
import Text.Show.Pretty
import Prelude hiding (Double, Float)

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6,
    _symbol :: Text,
    _price :: Amount 6
  }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makeLenses ''Lot

instance Splittable (Amount 6) Lot where
  howmuch = amount

totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

class HasLot f where
  _Lot :: Traversal' (f v) Lot

instance HasTraversal' HasLot fs => HasLot (Sum fs) where
  _Lot = traversing @HasLot _Lot
