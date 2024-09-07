{-# LANGUAGE DataKinds #-}

module Trade.Taxes.USA.WashSaleRule2 where

import Amount
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

data AddResult = AddResult
  { profitLoss :: Maybe (Amount 2),
    residual :: Maybe [Lot]
  }
  deriving (Eq, Show)

addLot :: Lot -> Lot -> AddResult
addLot x@(Lot xn xp xd) y@(Lot yn yp yd)
  | xn > 0 && yn < 0 || xn < 0 && yn > 0 =
      let n = xn + yn
          d = yp - xp
       in AddResult
            (Just (min (abs xn) (abs yn) * coerce d))
            ( if n == 0
                then Nothing
                else
                  Just
                    [ if abs xn > abs yn
                        then Lot (xn + yn) xp xd
                        else Lot (xn + yn) yp yd
                    ]
            )
  | xp == yp && xd == yd = AddResult Nothing (Just [Lot (xn + yn) xp xd])
  | otherwise = AddResult Nothing (Just [x, y])

identifyTrade :: Map a Lot -> Lot -> Map a Lot
identifyTrade ps l = undefined

identifyTrades :: (Ord a) => [Lot] -> Map a Lot
identifyTrades = foldl' identifyTrade mempty
