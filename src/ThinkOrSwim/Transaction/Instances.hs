{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Transaction.Instances where

import           Control.Lens
import           Data.Amount
import           Data.Coerce
import           Data.Ledger hiding (symbol, quantity, cost, price,
                                     washDeferred, washEligible)
import qualified Data.Ledger as L
import           Data.Time
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Types

type APIGainsKeeperState = GainsKeeperState API.TransactionSubType

type APICommodityLot = CommodityLot API.TransactionSubType

instance Transactional APICommodityLot where
    symbol       = L.symbol
    quantity     = L.quantity
    cost         = L.cost.non 0
    price        = L.price.non 0
    day          = purchaseDate.non (ModifiedJulianDay 0)
    loss f       = (<$ f 0)
    washDeferred = L.washDeferred
    washEligible = L.washEligible
    ident        = L.lotId

    washLoss _ _ = id
    clearLoss    = id

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

type APILotAndPL = LotAndPL API.TransactionSubType

instance Transactional APILotAndPL where
    symbol       = plLot.symbol
    quantity     = plLot.quantity
    cost         = plLot.cost
    price        = plLot.price
    day          = plDay.non (ModifiedJulianDay 0)
    loss         = plLoss
    washDeferred = plLot.washDeferred
    washEligible = plLot.washEligible
    ident        = plLot.ident

    washLoss b x y | b || abs (x^.quantity) == abs (y^.quantity) =
        y & loss         -~ x^.loss
          & cost         +~ coerce (x^.loss)
          & washEligible .~ False
          & plKind       .~ WashLoss
          & plLot.refs  <>~
              [ Ref (WashSaleRule (coerce (x^.loss))) (x^.ident) ]
    washLoss _ _ y = y

    clearLoss x | x^.plKind == WashLoss =
                    x & plKind .~ BreakEven
                      & loss .~ 0
                | otherwise = x

    isTransferIn x = isTransferIn (x^.plLot)

    arePaired     x y = (x^.plLot) `arePaired` (y^.plLot)
    areEquivalent x y = (x^.plLot) `areEquivalent` (y^.plLot)

    showPretty x = show (x^.plKind)
        ++ " " ++ showPretty (x^.plLot)
        ++ " $$ "  ++ show (x^.loss)
