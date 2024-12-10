{-# LANGUAGE DataKinds #-}

module Trade.Journal.Types where

import Amount
import Data.Map.Strict (Map)
import Data.Time

data TimePrice = TimePrice
  { price :: Amount 2,
    time :: UTCTime
  }
  deriving (Eq, Show)

data Lot = Lot
  { lotAmount :: Amount 2,
    lotDetail :: TimePrice
  }
  deriving (Eq, Show)

data Position
  = Open
      { openLot :: Lot,
        openBasis :: Maybe (Amount 2) -- if present, overrides lot price
      }
  | Closed
      { closingLot :: Lot,
        closingDetail :: TimePrice,
        closingWashable :: Bool -- if True, closing can be washed
      }
  deriving (Eq, Show)

-- This must be an involutive (@f . f = id@) function that reorders
-- transactions according to the order they should be closed in, and undoes
-- that ordering if called again. It is typically either the functions `id` or
-- `reverse`.
type Strategy = [Position] -> [Position]

newtype Journal a = Journal {getJournal :: [(a, Lot)]}
  deriving (Eq, Show)

newJournal :: Journal a
newJournal = Journal []

newtype (Ord a) => Ledger a = Ledger {getLedger :: Map a [Position]}
  deriving (Eq, Show)

newLedger :: (Ord a) => Ledger a
newLedger = Ledger mempty
