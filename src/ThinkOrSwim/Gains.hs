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
import Data.Foldable (foldl')
import Data.Ledger as Ledger
import Data.Maybe (maybeToList)
import Data.Text (Text, unpack)
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

import Debug.Trace (trace)

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
    -- current would add to or deduct from those positions, then it closes as
    -- much of that previous positions as quantities dictate.
    let cst   = abs (t^.item.API.cost)
        l     = setEvent (coerce cst)
        pl    = calculatePL l hist
        pls   = pl^..losses.traverse.to (False,)
             ++ pl^..opening.traverse.to (review _Lot).to (True,)
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        pls'  = pls & partsOf (each._2) %~ handleFees fees'

    (doc, res) <- washSaleRule (t^.baseSymbol) pls'

    let hist' = pl^.history ++ res^..traverse.filtered fst._2.plLot

    openTransactions.at sym ?= hist'

    let res'  = map snd res
        res'' = case mnet of
            Nothing -> res'
            Just net ->
                let slip = - (net + sumLotAndPL res')
                in res' ++ [ LotAndPL Rounding slip newCommodityLot
                           | slip /= 0 ]

    when (sym == "") $
        traceCurrentState sym mnet l pls pls' hist hist' doc res' res''

    pure res''
  where
    sym = symbolName t

    setEvent cst = newCommodityLot
        & Ledger.instrument .~ case t^?instrument_._Just.assetType of
              Just API.Equity           -> Ledger.Equity
              Just MutualFund           -> Ledger.Equity
              Just (OptionAsset _)      -> Ledger.Option
              Just (FixedIncomeAsset _) -> Ledger.Bond
              Just (CashEquivalentAsset
                    CashMoneyMarket)    -> Ledger.MoneyMarket
              Nothing                   -> error "Unexpected"
        & Ledger.kind     .~ t^.transactionInfo_.transactionSubType
        & Ledger.symbol   .~ symbolName t
        & Ledger.quantity .~
              coerce (case t^.item.instruction of Just Sell -> -n; _ -> n)
        & Ledger.cost  ?~ cst
        & Ledger.price    .~ coerce (t^.item.API.price)
        & Ledger.refs     .~ [ transactionRef t ]

traceCurrentState
    :: Text
    -> Maybe (Amount 2)
    -> CommodityLot API.TransactionSubType API.Transaction
    -> [(Bool, LotAndPL API.TransactionSubType API.Transaction)]
    -> [(Bool, LotAndPL API.TransactionSubType API.Transaction)]
    -> [CommodityLot API.TransactionSubType API.Transaction]
    -> [CommodityLot API.TransactionSubType API.Transaction]
    -> Doc
    -> [LotAndPL API.TransactionSubType API.Transaction]
    -> [LotAndPL API.TransactionSubType API.Transaction]
    -> State (GainsKeeperState API.TransactionSubType API.Transaction) ()
traceCurrentState sym mnet l pls pls' hist hist' doc res' res'' = do
    hist'' <- use (openTransactions.at sym.non [])
    traceM $ render
        $ text (unpack sym) <> text ": " <> text (showCommodityLot l)
       $$ text " mnet  > " <> text (show mnet)
       $$ text " pls   > " <> renderList (text . show) (map snd pls)
       $$ text " pls'  > " <> renderList (text . show) (map snd pls')
       $$ text " hist  > " <> renderList (text . showCommodityLot) hist
       $$ text " hist' > " <> renderList (text . showCommodityLot) hist'
       $$ text " hist''> " <> renderList (text . showCommodityLot) hist''
       $$ text " washed> " <> doc
       $$ text " res'  > " <> renderList (text . show) res'
       $$ text " res'' > " <> renderList (text . show) res''

calculatePL :: CommodityLot API.TransactionSubType API.Transaction
            -> [CommodityLot API.TransactionSubType API.Transaction]
            -> CalculatedPL
calculatePL curr opens = CalculatedPL {..}
  where
    ( maybeToList -> _opening,
      reverse     -> _losses,
      reverse     -> _history ) = foldl' fifo (Just curr, [], []) opens

    fifo (Nothing, res, keep) x = (Nothing, res, x:keep)
    fifo (Just z,  res, keep) x =
        ( _close^?_SplitKept
        , maybe res ((:res) . LotAndPL knd _loss) (_wasOpen^?_SplitUsed)
        , maybe keep (:keep) (_wasOpen^?_SplitKept)
        )
      where
        -- jww (2020-04-17): What about LossLong and GainLong?
        knd | _loss > 0 = LossShort
            | _loss < 0 = GainShort
            | otherwise = BreakEven

        LotApplied {..} = x `closeLot` z

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
closeLot :: CommodityLot API.TransactionSubType API.Transaction
         -> CommodityLot API.TransactionSubType API.Transaction
         -> LotApplied (CommodityLot API.TransactionSubType API.Transaction)
closeLot x y | not (pairedCommodityLots x y) = nothingApplied x y

closeLot x y | Just x' <- x^?effect, Just y' <- y^?effect, x' == y' =
    error $ show x ++ " has same position effect as " ++ show y
  where
    effect = refs._head.refOrig._Just.item.positionEffect._Just

closeLot x y = LotApplied {..}
  where
    (open', _close) = x `alignLots` y

    _loss :: Amount 2
    _loss | y^.kind == TransferOfSecurityOrOptionIn = 0
          | Just ocost <- _wasOpen^?_SplitUsed.Ledger.cost._Just,
            Just ccost <- _close^?_SplitUsed.Ledger.cost._Just =
                coerce (sign x ocost + sign y ccost)
          | otherwise = error "No cost found"

    _wasOpen = open'
        & _SplitUsed.quantity     %~ negate
        & _SplitUsed.Ledger.price .~ y^.Ledger.price

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
handleFees fee [l@(LotAndPL _ 0 x)] =
    [ l & plLot.Ledger.cost.mapped +~ coerce (sign x fee) ]

handleFees fee ls = go <$> spreadAmounts (^.plLot.quantity) fee ls
  where
    go (f, l) = l & plLoss +~ f
