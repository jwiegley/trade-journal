{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Taxes.USA.WashSaleRule2 where

import Amount
import Control.Exception (assert)
import Control.Lens
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Time

data Lot = Lot
  { _lotAmount :: Amount 2,
    _lotPrice :: Amount 6,
    _lotDate :: UTCTime
  }
  deriving (Eq, Show)

absLot :: Lot -> Lot
absLot (Lot amt p d) = Lot (abs amt) p d

negLot :: Lot -> Lot
negLot (Lot amt p d) = Lot (-amt) p d

data LotChange
  = AppendLots Lot Lot
  | IncreaseLot Lot
  | ReduceLot Lot Lot (Amount 2)
  | ReplaceLot Lot Lot (Amount 2)
  | CloseLot (Amount 2)
  deriving (Eq, Show)

addLot :: Lot -> Lot -> LotChange
addLot x@(Lot xn xp xd) y@(Lot yn yp yd)
  | xn > 0 && yn < 0 || xn < 0 && yn > 0 =
      let n = xn + yn
          s = min (abs xn) (abs yn)
          d = yp - xp
       in ( if n == 0
              then CloseLot
              else
                if abs xn > abs yn
                  then ReduceLot (Lot (xn + yn) xp xd) (Lot (-yn) xp xd)
                  else ReplaceLot (Lot (xn + yn) yp yd) (Lot xn xp xd)
          )
            ((if xn < 0 then -s else s) * coerce d)
  | xp == yp && xd == yd =
      IncreaseLot (Lot (xn + yn) xp xd)
  | otherwise =
      AppendLots x y

data Strategy
  = LIFO
  | FIFO
  deriving (Eq, Show)

data Position
  = Open Lot
  | Closed Lot (Amount 2)
  deriving (Eq, Show)

addToPositions :: Strategy -> Lot -> [Position] -> [Position]
addToPositions s x xs = strategy s (go x (strategy s xs))
  where
    strategy LIFO = reverse
    strategy FIFO = id

    go y [] = [Open y]
    go y (Open z : zs) = case z `addLot` y of
      AppendLots z' y' -> assert (y == y') $ Open z' : go y' zs
      IncreaseLot w -> Open w : zs
      ReduceLot w z' gain -> Closed z' gain : Open w : zs
      ReplaceLot w z' gain -> Closed z' gain : go w zs
      CloseLot gain -> Closed z gain : zs
    go y (c : zs) = c : go y zs

identifyTrade :: (Ord a) => Map a [Position] -> a -> Lot -> Map a [Position]
identifyTrade m sym lot = m & at sym %~ Just . go
  where
    go Nothing = [Open lot]
    go (Just ps) = addToPositions FIFO lot ps

identifyTrades :: (Ord a) => [(a, Lot)] -> Map a [Position]
identifyTrades = foldl' (uncurry . identifyTrade) mempty
