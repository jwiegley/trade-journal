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
import Data.Sum.Lens
import Text.Show.Pretty
import Prelude hiding (Double, Float)

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: Amount 6, -- number of shares, units, tokens, etc.
    _symbol :: Text, -- CUSIP, token name, etc.
    _price :: Amount 6 -- price per unit
  }
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makeLenses ''Lot

instance Splittable (Amount 6) Lot where
  howmuch = amount

-- | Given a lot and a per-share price, computed the totaled amount. This is
--   used for things like computing the total fees, etc.
totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

-- | 'HasLot' is a type class for indicating that all members of an open type
--   support the '_Lot' traversal.
class HasLot f where
  _Lot :: Traversal' (f v) Lot

instance HasTraversal' HasLot fs => HasLot (Sum fs) where
  _Lot = traversing @HasLot _Lot
