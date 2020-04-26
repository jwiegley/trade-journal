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

type APIGainsKeeperState =
    GainsKeeperState API.TransactionSubType API.Transaction

type APICommodityLot = CommodityLot API.TransactionSubType API.Transaction

instance Transactional APICommodityLot where
    symbol    = L.symbol
    quantity  = L.quantity
    cost      = L.cost.non 0
    price     = L.price.non 0
    day       = purchaseDate.non (ModifiedJulianDay 0)
    loss f    = (<$ f 0)
    washLoss  = const id
    clearLoss = id

    isWashEligible = view washEligible
    isTransferIn x = x^.kind == API.TransferOfSecurityOrOptionIn

    arePaired = pairedCommodityLots
    areEquivalent x y = x^.instrument == L.Equity &&
                        y^.instrument == L.Equity

    showPretty CommodityLot {..} =
        show _quantity
            ++ case _cost of
                   Nothing -> ""
                   Just xs -> " @@ " ++ show (xs / abs _quantity)
            ++ case _purchaseDate of
                   Nothing -> ""
                   Just d  -> " ## " ++ iso8601Show d

type APILotAndPL = LotAndPL API.TransactionSubType API.Transaction

instance Transactional APILotAndPL where
    symbol   = plLot.symbol
    quantity = plLot.quantity
    cost     = plLot.cost
    price    = plLot.price
    day      = plDay.non (ModifiedJulianDay 0)
    loss     = plLoss

    washLoss x y | abs (x^.quantity) == abs (y^.quantity) =
        y & loss   .~ - (x^.loss)
          & cost   +~ coerce (x^.loss)
          & plKind .~ WashLoss
          & plLot.washEligible .~ False
          & plLot.refs <>~
                [ Ref (WashSaleRule (coerce (x^.loss)))
                      (x^?!plLot.refs._head.refId)
                      (x^?!plLot.refs._head.refOrig) ]
    washLoss _ y = y

    clearLoss x | x^.plKind == WashLoss =
                    x & plKind .~ BreakEven & loss .~ 0
                | otherwise = x

    isWashEligible x = isWashEligible (x^.plLot)
    isTransferIn x = isTransferIn (x^.plLot)

    arePaired     x y = (x^.plLot) `arePaired` (y^.plLot)
    areEquivalent x y = (x^.plLot) `areEquivalent` (y^.plLot)

    showPretty x = show (x^.plKind)
        ++ " " ++ showPretty (x^.plLot)
        ++ " $$ "  ++ show (x^.loss)
