{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Journal.Types.Lot where

import Amount
import Control.Lens
import Control.Monad (when)
import Control.Monad.State
import Data.Data
import Data.Default
import Data.Text (Text)
import GHC.Generics hiding (to)
import GHC.TypeLits
import Text.Show.Pretty
import Trade.Journal.Split
import Prelude hiding (Double, Float)

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot = Lot
  { _amount :: !(Amount 6), -- number of shares, units, tokens, etc.
    _symbol :: !Text, -- CUSIP, token name, etc.
    _price :: !(Amount 6) -- price per unit
  }
  deriving (Show, PrettyVal, Eq, Ord, Generic, Data)

makeLenses ''Lot

instance Default Lot where
  def =
    Lot
      { _amount = 0,
        _symbol = "",
        _price = 0
      }

instance Splittable (Amount 6) Lot where
  howmuch = amount

-- | Given a lot and a per-share price, computed the totaled amount. This is
--   used for things like computing the total fees, etc.
totaled :: KnownNat n => Lot -> Amount n -> Amount n
totaled lot n = lot ^. amount . coerced * n

-- | 'HasLot' is a type class for indicating that all members of an open type
--   support the '_Lot' traversal.
class HasLot a where
  _Lot :: Traversal' a Lot

foldOver :: (Splittable n s, Show s) => State s a -> s -> [a]
foldOver f lot
  | lot' ^. howmuch >= lot ^. howmuch =
      error $ "Lot was not reduced: " ++ ppShow lot
  | lot' ^. howmuch == 0 = [a]
  | otherwise = a : foldOver f lot'
  where
    (a, lot') = runState f lot

foldOverM :: (Splittable n s, Show s, Monad m) => StateT s m a -> s -> m [a]
foldOverM f lot = do
  (a, lot') <- runStateT f lot
  when (lot' ^. howmuch >= lot ^. howmuch) $
    error $
      "Lot was not reduced: " ++ ppShow lot
  if lot' ^. howmuch == 0
    then pure [a]
    else (a :) <$> foldOverM f lot'
