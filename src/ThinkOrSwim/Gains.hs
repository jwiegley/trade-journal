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
import qualified Data.Ledger as L
import           Data.Ledger hiding (quantity, amount, cost, price)
import           Data.Split
import           Data.Text (Text, unpack)
import           Data.Time
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions hiding (cost, price)
import           ThinkOrSwim.Options (Options)
import qualified ThinkOrSwim.Options as Opts
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Transaction.Instances ()
import           ThinkOrSwim.Types
import           ThinkOrSwim.Wash

-- The function seeks to replicate logic used by GainsKeeper to calculate the
-- capital gains for a transaction based on a history of past transactions.
gainsKeeper
    :: Options
    -> Maybe (Amount 2)
    -> API.Transaction
    -> Amount 6
    -> State (GainsKeeperState API.TransactionSubType API.Transaction)
            [LotAndPL API.TransactionSubType API.Transaction]
gainsKeeper opts _ t n
    | not (Opts.capitalGains opts) =
      pure [ let pl = _Lot # createLot t n
             in pl & cost +~ coerce (sign (pl^.quantity) (transactionFees t)) ]

gainsKeeper opts mnet t n = do
    hist <- use (openTransactions.at sym.non [])

    -- If there are no existing lots, then this is either a purchase or a
    -- short sale. If there are existing lots for this symbol, then if the
    -- current transaction would add to or deduct from those positions, then
    -- it closes as much of those previous positions as quantities dictate.
    let l    = createLot t n
        pl   = consider closeLot mkLotAndPL hist l
        f x  = x & plLot.kind    .~ t^.xactSubType
                 & plLot.L.price .~ fmap coerce (t^.item.API.price)
        pls  = pl^..fromList.traverse.to f.to (False,)
            ++ pl^..newElement.traverse.to (review _Lot).to (True,)
        pls' = pls & partsOf (each._2) %~ handleFees (transactionFees t)

    res <- zoom (positionEvents.at (t^.baseSymbol).non []) $
        -- jww (2020-04-20): TD Ameritrade doesn't seem to apply the wash
        -- sale rule on purchase of a call contract after an equity loss.
        if Opts.washSaleRule opts && l^.instrument == L.Equity
        then pls' & traverse._2 %%~ washSaleRule
                 <&> \xs -> [ (a, b) | (a, bs) <- xs, b <- bs ]
        else pure pls'

    let hist' = pl^.newList ++ res^..traverse.filtered fst._2.plLot

    openTransactions.at sym ?= hist'

    let res'  = map snd res
        res'' = case mnet of
            Nothing -> res'
            Just net ->
                let slip = - (net + sumTransactions res')
                in res' ++ [ LotAndPL Rounding Nothing slip newCommodityLot
                           | slip /= 0 && abs slip < 0.02 ]

    when (sym == "") $
        traceCurrentState sym mnet l pls pls' hist hist' res''

    pure res''
  where
    sym = symbolName t

transactionFees :: API.Transaction-> Amount 2
transactionFees t
    = t^.fees_.regFee
    + t^.fees_.otherCharges
    + t^.fees_.commission

createLot :: API.Transaction -> Amount 6
          -> CommodityLot API.TransactionSubType API.Transaction
createLot t n = newCommodityLot @API.TransactionSubType
    & instrument      .~ instr
    & kind            .~ t^.xactSubType
    & L.quantity .~ quant
    & L.symbol   .~ symbolName t
    & L.cost     ?~ coerce (abs (t^.item.API.cost))
    & purchaseDate    ?~ utctDay (t^.xactDate)
    & refs            .~ [ transactionRef t ]
    & L.price    .~ fmap coerce (t^.item.API.price)
  where
    quant = coerce (case t^.item.instruction of Just Sell -> -n; _ -> n)

    instr = case t^?instrument_._Just.assetType of
        Just API.Equity           -> L.Equity
        Just MutualFund           -> L.Equity
        Just (OptionAsset _)      -> L.Option
        Just (FixedIncomeAsset _) -> L.Bond
        Just (CashEquivalentAsset
              CashMoneyMarket)    -> L.MoneyMarket
        Nothing                   -> error "Unexpected"

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a sell to close; a buy to close for the reverse. If both have the same
-- sign, nothing is done. If the cost basis per share of the two are
-- different, there will be a gain (or less, if negative). Also, we need to
-- return the part of 'x' that remains to be further deducted from, and how
-- much was consumed, and similarly for 'y'.
closeLot :: Transactional a => a -> a -> Applied (Amount 2) a a
closeLot x y | not (arePaired x y) = nothingApplied x y
             | otherwise         = Applied {..}
  where
    (src', _dest) = x `alignLots` y

    _src = src' & _SplitUsed.quantity %~ negate
                & _SplitUsed.price    .~ x^.price

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
    [ l & cost +~ coerce (sign (l^.quantity) fee) ]
handleFees fee ls =
    applyTo loss <$> spreadAmounts (^.quantity) fee ls

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
    renderM $ text (unpack sym) <> text ": " <> text (showPretty l)
       $$ text " nt> " <> text (show mnet)
       $$ text " p0> " <> renderList (text . showPretty) (map snd pls)
       $$ text " p1> " <> renderList (text . showPretty) (map snd pls')
       $$ text " h0> " <> renderList (text . showPretty) hist
       $$ text " h1> " <> renderList (text . showPretty) hist'
       $$ text " h2> " <> renderList (text . showPretty) hist''
       $$ text " rs> " <> renderList (text . showPretty) res''
