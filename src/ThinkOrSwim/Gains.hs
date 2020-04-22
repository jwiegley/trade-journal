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

module ThinkOrSwim.Gains where

import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Coerce
import Data.Ledger as Ledger
import Data.Split
import Data.Text (Text, unpack)
import Data.Time
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

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

    -- jww (2020-04-20): TD Ameritrade doesn't seem to apply the wash sale
    -- rule on purchase of a call contract after an equity loss.
    res <- if l^.Ledger.instrument == Ledger.Equity
          then washSaleRule (t^.baseSymbol) pls'
          else pure pls'

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
        & Ledger.instrument   .~ instr
        & Ledger.kind         .~ t^.transactionInfo_.transactionSubType
        & Ledger.quantity     .~ quant
        & Ledger.symbol       .~ symbolName t
        & Ledger.cost         ?~ cst
        & Ledger.purchaseDate ?~ utctDay (t^.xactDate)
        & Ledger.refs         .~ [ transactionRef t ]
        & Ledger.price        .~ coerce (t^.item.API.price)
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

calculatePL :: [CommodityLot API.TransactionSubType API.Transaction]
            -> CommodityLot API.TransactionSubType API.Transaction
            -> CalculatedPL
calculatePL = consider closeLot mk
  where
    mk c pl = LotAndPL knd (c^.purchaseDate) pl
      where
        knd | pl > 0    = LossShort
            | pl < 0    = GainShort
            | otherwise = BreakEven

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a sell to close; a buy to close for the reverse. If both have the same
-- sign, nothing is done. If the cost basis per share of the two are
-- different, there will be a gain (or less, if negative). Also, we need to
-- return the part of 'x' that remains to be further deducted from, and how
-- much was consumed, and similarly for 'y'.
closeLot :: CommodityLot API.TransactionSubType API.Transaction
         -> CommodityLot API.TransactionSubType API.Transaction
         -> Applied (Amount 2)
                   (CommodityLot API.TransactionSubType API.Transaction)
                   (CommodityLot API.TransactionSubType API.Transaction)
closeLot x y | not (pairedCommodityLots x y) = nothingApplied x y

closeLot x y | Just x' <- x^?effect, Just y' <- y^?effect, x' == y' =
    error $ show x ++ " has same position effect as " ++ show y
  where
    effect = refs._head.refOrig._Just.item.positionEffect._Just

closeLot x y = Applied {..}
  where
    (src', _dest) = x `alignLots` y

    _src = src'
        & _SplitUsed.quantity     %~ negate
        & _SplitUsed.Ledger.price .~ y^.Ledger.price

    _value :: Amount 2
    _value | y^.kind == TransferOfSecurityOrOptionIn = 0
          | Just ocost <- _src^?_SplitUsed.Ledger.cost._Just,
            Just ccost <- _dest^?_SplitUsed.Ledger.cost._Just =
                coerce (sign x ocost + sign y ccost)
          | otherwise = error "No cost found"

-- Handling fees is a touch tricky, since if we end up closing multiple
-- positions via a single sale or purchase, the fees are applied across all of
-- them. Yet if the fee is oddly divided, we must carry the remaining penny,
-- since otherwise .03 divided by 2 (for example) will round to two instances
-- of .01. See tests for examples.
--
-- If there is only a single opening transaction to apply the fee to, we add
-- it directly to the basis cost, rather than spread it across the multiple
-- transactions.
handleFees :: Amount 2 -> [LotAndPL k t] -> [LotAndPL k t]
handleFees fee [l@(LotAndPL _ _ 0 x)] =
    [ l & plLot.Ledger.cost.mapped +~ coerce (sign x fee) ]

handleFees fee ls = go <$> spreadAmounts (^.plLot.quantity) fee ls
  where
    go (f, l) = l & plLoss +~ f
