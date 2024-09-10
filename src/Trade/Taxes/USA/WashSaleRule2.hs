{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Taxes.USA.WashSaleRule2 where

import Amount
import Control.Lens
import Data.Map (Map)
import Data.Time

data TimePrice = TimePrice
  { price :: Amount 6,
    time :: UTCTime
  }
  deriving (Eq, Show)

data Lot = Lot
  { lotAmount :: Amount 2,
    lotDetail :: TimePrice
  }
  deriving (Eq, Show)

absLot :: Lot -> Lot
absLot (Lot amt d) = Lot (abs amt) d

negLot :: Lot -> Lot
negLot (Lot amt d) = Lot (-amt) d

data LotChange
  = AddLot Lot
  | ReduceLot (Either Lot Lot)
  deriving (Eq, Show)

addLot :: Lot -> Lot -> Maybe LotChange
addLot (Lot xn xd) (Lot yn yd)
  | xn > 0 && yn < 0 || xn < 0 && yn > 0 =
      Just
        ( ReduceLot
            ( if abs xn >= abs yn
                then Left (Lot (xn + yn) xd)
                else Right (Lot (xn + yn) yd)
            )
        )
  | xd == yd = Just (AddLot (Lot (xn + yn) xd))
  | otherwise = Nothing

data Position
  = Open Lot
  | Closed
      { closedLot :: Lot,
        closedDetail :: TimePrice
      }
  deriving (Eq, Show)

-- This must be an involutive (@f . f = id@) function that reorders
-- transactions according to the order they should be closed in, and undoes
-- that ordering if called again.
type Strategy = [Position] -> [Position]

addToPositions :: Strategy -> Lot -> [Position] -> [Position]
addToPositions strategy x xs = strategy (go x (strategy xs))
  where
    go y [] = [Open y]
    go y@(Lot _yn yd) (Open z@(Lot zn _zd) : zs) =
      case z `addLot` y of
        Nothing -> Open z : go y zs
        Just (AddLot w) -> Open w : zs
        Just (ReduceLot (Left z'@(Lot zn' zd'))) ->
          Open z' : Closed (Lot (zn - zn') zd') yd : zs
        Just (ReduceLot (Right w)) ->
          Closed z yd : go w zs
    go y (c : zs) = c : go y zs

identifyTrade ::
  (Ord a) =>
  Strategy ->
  Map a [Position] ->
  a ->
  Lot ->
  Map a [Position]
identifyTrade strategy m sym lot = m & at sym %~ Just . go
  where
    go Nothing = [Open lot]
    go (Just ps) = addToPositions strategy lot ps

identifyTrades ::
  (Ord a) =>
  Strategy ->
  Map a [Position] ->
  [(a, Lot)] ->
  Map a [Position]
identifyTrades strategy = foldl' (uncurry . identifyTrade strategy)
