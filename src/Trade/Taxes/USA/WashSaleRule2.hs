{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Taxes.USA.WashSaleRule2 where

import Amount
import Control.Lens
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Time
import Trade.Data.Zipper

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

data Position
  = Open
      { openLot :: Lot,
        openBasis :: Maybe (Amount 6) -- if present, overrides lot price
      }
  | Closed
      { closingLot :: Lot,
        closingDetail :: TimePrice,
        closingWashable :: Bool -- if True, closing can be washed
      }
  deriving (Eq, Show)

-- This must be an involutive (@f . f = id@) function that reorders
-- transactions according to the order they should be closed in, and undoes
-- that ordering if called again.
type Strategy = [Position] -> [Position]

data LotChange
  = AddLot Lot
  | -- | When reducing a lot, either some of lot 'x' is reduced by lot 'y',
    --   left a 'Left' remainder, or all of 'x' is reduced and we have part of
    --   'y' as a 'Right' remainder.
    ReduceLot (Either Lot Lot)
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

addToPositions :: Strategy -> Lot -> [Position] -> [Position]
addToPositions strategy x xs = strategy (go x (strategy xs))
  where
    go y [] = [Open y Nothing]
    go y@(Lot _yn yd) (Open z@(Lot zn _zd) wash : zs) =
      case z `addLot` y of
        Nothing -> Open z wash : go y zs
        Just (AddLot w) -> Open w wash : zs
        Just (ReduceLot (Left z'@(Lot zn' zd'))) ->
          Open z' wash
            : Closed (Lot (zn - zn') zd') yd True
            : zs
        Just (ReduceLot (Right w)) ->
          Closed z yd True : go w zs
    go y (c : zs) = c : go y zs

identifyTrades ::
  (Ord a) =>
  Strategy ->
  Map a [Position] ->
  [(a, Lot)] ->
  Map a [Position]
identifyTrades strategy = foldl' (uncurry . identifyTrade)
  where
    identifyTrade m sym lot = m & at sym %~ Just . go
      where
        go Nothing = [Open lot Nothing]
        go (Just ps) = addToPositions strategy lot ps

-- | A wash sale happens when you sell a security at a loss and buy a
--   “substantially identical” security within 30 days before or after the
--   sale.
--
--   The two basic scenarios are as follows, where opening B and closing A are
--   within 30 days of each other:
--
--     Open A --> Open B --> Close A at loss
--                ^---- wash
--
--     Open B --> Open A --> Close A at loss
--          ^---------- wash
--
--     Open A --> Close A at loss --> Open B
--                                ----^ wash
--
--   In both cases, the position A is no longer open, the closing of A is
--   considered "washed", and the opening of B has had its cost basis
--   adjusted.
washSales :: [Position] -> [Position]
washSales = survey go
  where
    go
      ( MkZipper
          before
          event@( Closed
                    l@(Lot n (TimePrice b d))
                    pd@(TimePrice p _)
                    True
                  )
          after
        )
        | eligibleLosingClose event =
            case break (eligibleNewOpen d) before of
              (xpre, Open x Nothing : xpost) ->
                MkZipper (xpre ++ adjusted x : xpost) closed after
              _ -> case break (eligibleNewOpen d) after of
                (ypre, Open y Nothing : ypost) ->
                  MkZipper before closed (ypre ++ adjusted y : ypost)
                _ -> justClosed
        | otherwise = justClosed
        where
          totalLoss :: Amount 6
          totalLoss = coerce n * (b - p)
          adjusted x@(Lot m (TimePrice o _)) =
            Open x (Just (o + totalLoss / coerce m))
          closed = Closed l pd False
          justClosed = MkZipper before closed after
    go z = z

    eligibleLosingClose
      (Closed (Lot n (TimePrice p _)) (TimePrice p' _) True) =
        coerce n * (p' - p) < 0
    eligibleLosingClose _ = False

    eligibleNewOpen d (Open (Lot _ (TimePrice _ d')) Nothing) =
      d /= d' && withinDays 30 d d'
    eligibleNewOpen _ _ = False

withinDays :: Integer -> UTCTime -> UTCTime -> Bool
withinDays days x y = x `diffUTCTime` y < fromIntegral days * 86400

-- | A Closing represents a gain or loss depends on the difference between the
--   cost of the lot and the price of the sale. The difference in time
--   indicates either a long term (> 1 year) or short term capital gain.
isLongTermGain :: Lot -> TimePrice -> Bool
isLongTermGain (Lot _ (TimePrice _ x)) (TimePrice _ y) = withinDays 365 y x
