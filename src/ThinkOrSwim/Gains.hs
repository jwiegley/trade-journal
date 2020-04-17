{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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
import Prelude hiding (Float, Double)
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
import ThinkOrSwim.Wash

-- The function seeks to replicate the logic used by GainsKeeper to determine
-- what impact a given transaction, based on existing positions, should have
-- on an account.
gainsKeeper
    :: API.Transaction -> CommodityLot API.Transaction
    -> State (GainsKeeperState API.Transaction) [LotAndPL API.Transaction]
gainsKeeper t cl = do
    hist <- use (openTransactions.at sym.non [])

    -- If there are no existing lots, then this is either a purchase or a
    -- short sale. If there are existing lots for this symbol, then if the
    -- current would add to or deduct from those positions, then it closes as
    -- much of that previous positions as quantities dictate.
    let cst   = abs (t^.item.API.cost)
        l     = setEvent (coerce cst)
        pl    = calculatePL l hist
        pls   = pl^..losses.traverse.to (False,)
             ++ pl^..opening.traverse.to (LotAndPL 0).to (True,)
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        wfees = handleFees fees' pls
        res   = wfees^..traverse._2
        hist' = pl^.history ++ wfees^..traverse.filtered fst._2.lot

    openTransactions.at sym ?= hist'

    traceCurrentState sym l hist hist' =<< washSaleRule res
  where
    sym = cl^.Ledger.symbol

    setEvent cst = cl
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId) t]

traceCurrentState
    :: Text
    -> CommodityLot API.Transaction
    -> [CommodityLot API.Transaction]
    -> [CommodityLot API.Transaction]
    -> [LotAndPL API.Transaction]
    -> State (GainsKeeperState API.Transaction) [LotAndPL API.Transaction]
traceCurrentState sym l hist next res = do
    hist' <- use (openTransactions.at sym.non [])
    traceM $ render
           $ renderHistoryBeforeAndAfter
                 (unpack sym) l hist next res hist'
    return res

calculatePL :: CommodityLot API.Transaction
            -> [CommodityLot API.Transaction]
            -> CalculatedPL
calculatePL curr opens = CalculatedPL {..}
  where
    ( maybeToList -> _opening,
      reverse     -> _losses,
      reverse     -> _history ) = foldl' fifo (Just curr, [], []) opens

    fifo (Nothing, res, keep) x = (Nothing, res, x:keep)
    fifo (Just z, res, keep)  x =
        ( _close^?_SplitKept
        , maybe res ((:res) . LotAndPL _gain) (_wasOpen^?_SplitUsed)
        , maybe keep (:keep) (_wasOpen^?_SplitKept)
        )
      where
        LotApplied {..} = x `closeLot` z

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
closeLot :: CommodityLot API.Transaction -> CommodityLot API.Transaction
         -> LotApplied API.Transaction
closeLot x y | not (pairedCommodityLots x y) =
    LotApplied 0.0 (None x) (None y)

closeLot x y
    | Just x' <- x^?refs._head.refOrig.item.positionEffect._Just,
      Just y' <- y^?refs._head.refOrig.item.positionEffect._Just,
      x' == y' =
    error $ show x ++ " has same position effect as " ++ show y

closeLot x y' = LotApplied {..}
  where
    y | isTransactionSubType OptionExpiration y' =
        y' & quantity %~ negate
      | otherwise = y'

    (open', _close) = x `alignLots` y

    _gain :: Amount 2
    _gain | isTransactionSubType TransferOfSecurityOrOptionIn y = 0
          | Just ocost <- _wasOpen^?_SplitUsed.Ledger.cost._Just,
            Just ccost <- _close^?_SplitUsed.Ledger.cost._Just =
            coerce (normalizeAmount mpfr_RNDN
                        (coerce (sign x ocost + sign y ccost) :: Amount 3))
          | otherwise = 0

    _wasOpen = open'
        & _SplitUsed.quantity     %~ negate
        & _SplitUsed.Ledger.price .~ y^.Ledger.price

-- Handling fees is a touch tricky, since if we end up closing multiple
-- positions via a single sale or purchase, the fees are applied across all of
-- them. Yet if the fee is oddly divided, we must carry the remaining penny,
-- since otherwise .03 divided by 2 (for example) will round to two instances
-- of .01. See tests for examples.
handleFees :: forall t a. Amount 2 -> [(a, LotAndPL t)] -> [(a, LotAndPL t)]

-- If there is only a single transaction to apply the fee to, we add (or
-- subtract) it directly to (from) the basis cost, rather than dividing it
-- into the P/L of multiple transactions.
handleFees fee [(w, LotAndPL 0.0 x)] =
    [(w, LotAndPL 0.0 $ x & Ledger.cost.mapped +~
             coerce (if x^.quantity < 0 then (-fee) else fee))]

handleFees fee lots = go True lots
  where
    go _ [] = []
    go b ((w, LotAndPL g x):xs) = (w, LotAndPL (g' + sum') x) : go False xs
      where
        g'   = normalizeAmount mpfr_RNDNA g
        sum' = normalizeAmount mpfr_RNDZ (sumOfParts x + if b then diff else 0)
        diff = fee - sum (map (sumOfParts . view lot . snd) lots)

        sumOfParts l =
            normalizeAmount mpfr_RNDZ (coerce (l^.quantity * perShare))
          where
            perShare = coerce fee / shares
            shares   = sum (map (^._2.lot.quantity) lots)
