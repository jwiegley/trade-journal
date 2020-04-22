{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Transaction where

import           Control.Lens
import           Data.Amount
import           Data.Coerce
import           Data.Ledger hiding (quantity, cost, price)
import qualified Data.Ledger as Ledger
import           Data.Split
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Types
import           ThinkOrSwim.Wash as Wash

instance Transactional
    (CommodityLot API.TransactionSubType API.Transaction) where
    quantity = Ledger.quantity
    cost     = Ledger.cost.non 0
    price    = Ledger.price.non 0
    day      = purchaseDate.non (ModifiedJulianDay 0)
    loss f p = p <$ f 0

    washLoss x y = (x, y)
    unwash x = x

    isTransferIn x = x^.kind == API.TransferOfSecurityOrOptionIn

    arePaired = pairedCommodityLots
    areEquivalent x y = x^.instrument == Ledger.Equity &&
                        y^.instrument == Ledger.Equity

    align = alignLots
    showPretty = show

instance Transactional
    (LotAndPL API.TransactionSubType API.Transaction) where
    quantity = plLot.Wash.quantity
    cost     = plLot.Wash.cost
    price    = plLot.Wash.price
    day      = plDay.non (ModifiedJulianDay 0)
    loss     = plLoss

    washLoss x y
        | abs (x^.plLot.Ledger.quantity) ==
          abs (y^.plLot.Ledger.quantity) =
        ( x & plLoss .~ 0
        , y & plKind .~ WashLoss
            & plLoss .~ - (x^.plLoss)
            & plLot.Ledger.cost._Just +~ coerce (x^.plLoss)
            & plLot.refs <>~
                  [ Ref (WashSaleRule (coerce (x^.plLoss)))
                        (x^?!plLot.refs._head.refId)
                        (x^?!plLot.refs._head.refOrig)
                  ]
        )
    washLoss x y = (x, y)

    unwash x | x^.plKind == WashLoss =
                   x & plKind .~ BreakEven & plLoss .~ 0
             | otherwise = x

    isTransferIn x = isTransferIn (x^.plLot)

    arePaired x y = (x^.plLot) `arePaired` (y^.plLot)
    areEquivalent x y = (x^.plLot) `areEquivalent` (y^.plLot)

    align = alignPL
    showPretty = showLotAndPL

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a sell to close; a buy to close for the reverse. If both have the same
-- sign, nothing is done. If the cost basis per share of the two are
-- different, there will be a gain (or less, if negative). Also, we need to
-- return the part of 'x' that remains to be further deducted from, and how
-- much was consumed, and similarly for 'y'.
closeLot :: Transactional a => a -> a -> Applied (Amount 2) a a
closeLot x y | not (arePaired x y) = nothingApplied x y
closeLot x y = Applied {..}
  where
    (src', _dest) = x `align` y

    _src = src' & _SplitUsed.quantity %~ negate
                & _SplitUsed.price    .~ y^.price

    _value :: Amount 2
    _value | isTransferIn y = 0
           | Just ocost <- _src^?_SplitUsed.cost,
             Just ccost <- _dest^?_SplitUsed.cost =
                 coerce (sign (x^.quantity) ocost +
                         sign (y^.quantity) ccost)
           | otherwise = error "No cost found"

-- Handling fees is a touch tricky, since if we end up closing multiple
-- positions via a single sale or purchase, the fees are applied across all of
-- them. Yet if the fee is oddly divided, we must carry the remaining penny,
-- since otherwise .03 divided by 2 (for example) will round to two instances
-- of .01.
--
-- If there is only a single opening transaction to apply the fee to, we add
-- it directly to the basis cost, rather than spread it across the multiple
-- transactions.
handleFees :: Transactional a => Amount 2 -> [a] -> [a]
handleFees fee [l] | l^.loss == 0 =
    [ l & cost +~ coerce (if l^.quantity < 0 then -fee else fee) ]
handleFees fee ls =
    (\(f, l) -> l & loss +~ f) <$> spreadAmounts (^.quantity) fee ls
