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

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import qualified Data.Ledger as L
import           Data.Ledger hiding (symbol, quantity, amount, cost, price)
import           Data.Split
import           Data.Text (Text, unpack)
import           Data.Utils
import           Debug.Trace (traceM)
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import           ThinkOrSwim.Options (Options)
import qualified ThinkOrSwim.Options as Opts
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Transaction.Instances
import           ThinkOrSwim.Types
import           ThinkOrSwim.Wash

-- The function seeks to replicate logic used by GainsKeeper to calculate the
-- capital gains for a transaction based on a history of past transactions.
gainsKeeper :: Options -> Amount 2 -> Maybe (Amount 2) -> APICommodityLot
            -> State APIGainsKeeperState [APILotAndPL]
gainsKeeper opts fees _ l
    | not (Opts.capitalGains opts) =
      pure [ _LotAndPL # l & cost +~ coerce (sign (l^.quantity) fees) ]

gainsKeeper opts fees mnet l = do
    let sym = l^.symbol

    hist <- use (openTransactions.at sym.non [])

    -- If there are no existing lots, then this is either a purchase or a
    -- short sale. If there are existing lots for this symbol, then if the
    -- current transaction would add to or deduct from those positions, then
    -- it closes as much of those previous positions as quantities dictate.
    let c    = consider closeLot mkLotAndPL hist l
        f x  = x & plLot.kind .~ l^.kind
                 & plLot.L.price %~ (fmap coerce (l^.L.price) <|>)
        pls  = c^..fromList.each.to f.to (False,)
            ++ c^..newElement.each.to (review _LotAndPL).to (True,)
        pls' = pls & partsOf (each._2) %~ handleFees fees

    openTransactions.at sym ?= c^.newList

    res <- zoom (symbolHistory sym) $
        -- jww (2020-04-20): TD Ameritrade doesn't seem to apply the wash
        -- sale rule on purchase of a call contract after an equity loss.
        if Opts.washSaleRule opts && l^.instrument == L.Equity
        then do
            res <- pls' & each._2 %%~ \pl -> do
                (doc, res) <- washSaleRule plLot (plLot.purchaseDate._Just) pl
                traceM $ render doc
                pure res
            pure $ concatMap sequenceA res
        else pure pls'

    let hist' = c^.newList ++ res^..each.filtered fst._2.plLot

    openTransactions.ix sym <>= res^..each.filtered fst._2.plLot

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

traceCurrentState :: Text
                  -> Maybe (Amount 2)
                  -> APICommodityLot
                  -> [(Bool, APILotAndPL)]
                  -> [(Bool, APILotAndPL)]
                  -> [APICommodityLot]
                  -> [APICommodityLot]
                  -> [APILotAndPL]
                  -> State APIGainsKeeperState ()
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
