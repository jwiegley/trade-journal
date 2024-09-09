{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Taxes.USA.WashSaleRule2 where

import Amount
-- import Control.Lens

-- import Control.Arrow ((***))

-- import Data.Maybe (fromMaybe)

import Control.Exception (assert)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Time

data Lot = Lot
  { _lotAmount :: Amount 2,
    _lotPrice :: Amount 6,
    _lotDate :: UTCTime
  }
  deriving (Eq, Show)

data LotChange
  = TwoLots Lot Lot
  | IncreaseLot Lot
  | ReduceLot Lot (Amount 2)
  | ReplaceLot Lot (Amount 2)
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
                ( if abs xn > abs yn
                    then ReduceLot
                    else ReplaceLot
                )
                  ( if abs xn > abs yn
                      then Lot (xn + yn) xp xd
                      else Lot (xn + yn) yp yd
                  )
          )
            ((if xn < 0 then -s else s) * coerce d)
  | xp == yp && xd == yd =
      IncreaseLot (Lot (xn + yn) xp xd)
  | otherwise =
      TwoLots x y

data Strategy
  = LIFO
  | FIFO
  deriving (Eq, Show)

data Position
  = Open Lot
  | Closed Lot (Amount 2)
  | PartialClose Lot Lot (Amount 2)
  deriving (Eq, Show)

addToLots :: Strategy -> Lot -> [Lot] -> [Position]
addToLots s x xs = go x $ case s of
  LIFO -> reverse xs
  FIFO -> xs
  where
    go y [] = [Open y]
    go y (z : zs) = case z `addLot` y of
      TwoLots z' y' -> assert (y == y') $ Open z' : go y' zs
      IncreaseLot w -> Open w : map Open zs
      ReduceLot w gain -> PartialClose z w gain : map Open zs
      ReplaceLot w gain -> Closed z gain : go w zs
      CloseLot gain -> Closed z gain : map Open zs

identifyTrade :: Map a [Lot] -> Lot -> Map a [Lot]
identifyTrade _ps _l = undefined

identifyTrades :: (Ord a) => [Lot] -> Map a [Lot]
identifyTrades = foldl' identifyTrade mempty
