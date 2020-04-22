{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Gains where

import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import qualified Data.Ledger as Ledger
import           Data.Ledger hiding (quantity, amount, cost, price)
import           Data.Split
import           Data.Text (Text, unpack)
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions hiding (cost, price)
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Types
import           ThinkOrSwim.Wash

-- The function seeks to replicate the logic used by GainsKeeper to determine
-- what impact a given transaction, based on existing positions, should have
-- on an account.
gainsKeeper
    :: Maybe (Amount 2)
    -> API.Transaction
    -> Amount 6
    -> State (GainsKeeperState API.TransactionSubType API.Transaction)
            [LotAndPL API.TransactionSubType API.Transaction]
gainsKeeper mnet t n = do
    hist <- use (openTransactions.at sym.non [])

    -- If there are no existing lots, then this is either a purchase or a
    -- short sale. If there are existing lots for this symbol, then if the
    -- current transaction would add to or deduct from those positions, then
    -- it closes as much of those previous positions as quantities dictate.
    let cst  = abs (t^.item.API.cost)
        l    = setEvent (coerce cst)
        pl   = calculatePL hist l
        pls  = pl^..fromList.traverse.to (False,)
            ++ pl^..newElement.traverse.to (review _Lot).to (True,)
        fees = t^.fees_.regFee
             + t^.fees_.otherCharges
             + t^.fees_.commission
        pls' = pls & partsOf (each._2) %~ handleFees fees

    res <- zoom (positionEvents.at (t^.baseSymbol).non []) $
        if l^.instrument == Ledger.Equity
        then pls' & traverse._2 %%~ washSaleRule
                 <&> concatMap sequenceA
        else
            -- jww (2020-04-20): TD Ameritrade doesn't seem to apply the wash
            -- sale rule on purchase of a call contract after an equity loss.
            pure pls'

    let hist' = pl^.newList ++ res^..traverse.filtered fst._2.plLot

    openTransactions.at sym ?= hist'

    let res'  = map snd res
        res'' = case mnet of
            Nothing -> res'
            Just net ->
                let slip = - (net + sumLotAndPL res')
                in res' ++ [ LotAndPL Rounding Nothing slip newCommodityLot
                           | slip /= 0 ]

    when (sym == "BAC") $
        traceCurrentState sym mnet l pls pls' hist hist' res''

    pure res''
  where
    sym = symbolName t

    setEvent cst = newCommodityLot @API.TransactionSubType
        & instrument      .~ instr
        & kind            .~ t^.transactionInfo_.transactionSubType
        & Ledger.quantity .~ quant
        & Ledger.symbol   .~ symbolName t
        & Ledger.cost     ?~ cst
        & purchaseDate    ?~ utctDay (t^.xactDate)
        & refs            .~ [ transactionRef t ]
        & Ledger.price    .~ coerce (t^.item.API.price)
      where
        quant = coerce (case t^.item.instruction of Just Sell -> -n; _ -> n)

        instr = case t^?instrument_._Just.assetType of
            Just API.Equity           -> Ledger.Equity
            Just MutualFund           -> Ledger.Equity
            Just (OptionAsset _)      -> Ledger.Option
            Just (FixedIncomeAsset _) -> Ledger.Bond
            Just (CashEquivalentAsset
                  CashMoneyMarket)    -> Ledger.MoneyMarket
            Nothing                   -> error "Unexpected"


calculatePL
    :: [CommodityLot API.TransactionSubType API.Transaction]
    -> CommodityLot API.TransactionSubType API.Transaction
    -> Considered (LotAndPL API.TransactionSubType API.Transaction)
                 (CommodityLot API.TransactionSubType API.Transaction)
                 (CommodityLot API.TransactionSubType API.Transaction)
calculatePL = consider closeLot mk
  where
    mk c pl = LotAndPL knd (c^.purchaseDate) pl
      where
        knd | pl > 0    = LossShort
            | pl < 0    = GainShort
            | otherwise = BreakEven

traceCurrentState
    :: Text
    -> Maybe (Amount 2)
    -> CommodityLot API.TransactionSubType API.Transaction
    -> [(Bool, LotAndPL API.TransactionSubType API.Transaction)]
    -> [(Bool, LotAndPL API.TransactionSubType API.Transaction)]
    -> [CommodityLot API.TransactionSubType API.Transaction]
    -> [CommodityLot API.TransactionSubType API.Transaction]
    -> [LotAndPL API.TransactionSubType API.Transaction]
    -> State (GainsKeeperState API.TransactionSubType API.Transaction) ()
traceCurrentState sym mnet l pls pls' hist hist' res'' = do
    hist'' <- use (openTransactions.at sym.non [])
    renderM $ text (unpack sym) <> text ": " <> text (showCommodityLot l)
       $$ text " nt> " <> text (show mnet)
       $$ text " p0> " <> renderList (text . showLotAndPL) (map snd pls)
       $$ text " p1> " <> renderList (text . showLotAndPL) (map snd pls')
       $$ text " h0> " <> renderList (text . showCommodityLot) hist
       $$ text " h1> " <> renderList (text . showCommodityLot) hist'
       $$ text " h2> " <> renderList (text . showCommodityLot) hist''
       $$ text " rs> " <> renderList (text . showLotAndPL) res''
