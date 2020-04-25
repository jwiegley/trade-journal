{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Transaction.Instances where

import           Control.Lens
import           Data.Amount
import           Data.Coerce
import           Data.Ledger hiding (symbol, quantity, cost, price)
import qualified Data.Ledger as L
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Types

instance Transactional
    (CommodityLot API.TransactionSubType API.Transaction) where
    symbol    = L.symbol
    quantity  = L.quantity
    cost      = L.cost.non 0
    price     = L.price.non 0
    day       = purchaseDate.non (ModifiedJulianDay 0)
    loss f    = (<$ f 0)
    washLoss  = const id
    clearLoss = id

    isTransferIn x = x^.kind == API.TransferOfSecurityOrOptionIn

    arePaired = pairedCommodityLots
    areEquivalent x y = x^.instrument == L.Equity &&
                        y^.instrument == L.Equity

    showPretty CommodityLot {..} =
        show _quantity
            ++ case _cost of
                   Nothing -> ""
                   Just xs -> " @@ " ++ show xs
            ++ case _purchaseDate of
                   Nothing -> ""
                   Just d  -> " ## " ++ iso8601Show d

instance Transactional
    (LotAndPL API.TransactionSubType API.Transaction) where
    symbol   = plLot.symbol
    quantity = plLot.quantity
    cost     = plLot.cost
    price    = plLot.price
    day      = plDay.non (ModifiedJulianDay 0)
    loss     = plLoss

    washLoss x y | abs (x^.plLot.L.quantity) ==
                   abs (y^.plLot.L.quantity) =
        y & plKind .~ WashLoss
          & plLoss .~ - (x^.plLoss)
          & plLot.L.cost._Just +~ coerce (x^.plLoss)
          & plLot.refs <>~
                [ Ref (WashSaleRule (coerce (x^.plLoss)))
                      (x^?!plLot.refs._head.refId)
                      (x^?!plLot.refs._head.refOrig) ]
    washLoss _ y = y

    clearLoss x | x^.plKind == WashLoss =
                      x & plKind .~ BreakEven & plLoss .~ 0
                | otherwise = x

    isTransferIn x = isTransferIn (x^.plLot)

    arePaired     x y = (x^.plLot) `arePaired` (y^.plLot)
    areEquivalent x y = (x^.plLot) `areEquivalent` (y^.plLot)

    showPretty x = show (x^.plKind)
        ++ " " ++ showPretty (x^.plLot)
        ++ " $$ "  ++ show (x^.plLoss)
