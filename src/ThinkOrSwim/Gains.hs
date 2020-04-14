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

module ThinkOrSwim.Gains where

import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Coerce
import Data.Foldable (foldlM)
import Data.Ledger as Ledger
import Data.Maybe (isJust, maybeToList)
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
-- import ThinkOrSwim.Wash

-- import Data.Text (unpack)
-- import Debug.Trace
-- import Text.PrettyPrint

-- The function seeks to replicate the logic used by GainsKeeper to determine
-- what impact a given transaction, based on existing positions, should have
-- on an account.
gainsKeeper
    :: API.Transaction -> CommodityLot API.Transaction
    -> State (GainsKeeperState API.Transaction) [LotAndPL API.Transaction]
gainsKeeper t cl = do
    let sym   = cl^.Ledger.symbol
        cst   = abs (t^.item.API.cost)
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission

    hist <- use (at sym.non (newEventHistory []))

    -- If there are no existing lots, then this is either a purchase or a
    -- short sale. If there are existing lots for this symbol, then if the
    -- current would add to or deduct from those positions, then it closes as
    -- much of that previous positions as quantities dictate.
    let ls   = hist^.openTransactions
        l    = reflectEvent (coerce cst)

    pl <- calculatePL l ls

    let res  = handleFees fees' (pl^.losses ++ maybeToList (LotAndPL 0 <$> pl^.leftover))
        next = case pl^.history ++ [res^?!_last.lot | isJust (pl^.leftover)] of
                   [] -> Nothing
                   xs -> Just $ hist & openTransactions .~ xs

    -- traceM $ render $ renderHistoryBeforeAndAfter (unpack sym) l hist res next

    at sym .= next
    pure res
  where
    reflectEvent cst = cl
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId) t]

calculatePL :: CommodityLot API.Transaction -> [CommodityLot API.Transaction]
            -> State (GainsKeeperState API.Transaction) CalculatedPL
calculatePL l ls = do
    (x, xs, ys) <- foldlM fifo (Just l, [], []) ls
    -- -- jww (2020-04-14): Calculate how gain is affected by the wash sale rule.
    -- washSale <- washSaleRule y'
    pure $ CalculatedPL (reverse xs) (reverse ys) x
  where
    fifo (Nothing, res, keep) x = pure (Nothing, res, x:keep)
    fifo (Just z, res, keep) x = do
        pure ( _left
             , maybe res ((:res) . (LotAndPL _gain)) _used
             , maybe keep (:keep) _kept
             )
      where
        LotApplied {..} = x `applyLot` z

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
applyLot :: CommodityLot API.Transaction -> CommodityLot API.Transaction
         -> LotApplied API.Transaction
applyLot x y
    |   x^.quantity < 0 && y^.quantity < 0
      || (  x^.quantity > 0 && y^.quantity > 0
         && not (isTransactionSubType OptionExpiration y)) =
    LotApplied 0.0 Nothing (Just x) (Just y)

applyLot x y
    | Just x' <- x^?refs._head.refOrig.item.positionEffect._Just,
      Just y' <- y^?refs._head.refOrig.item.positionEffect._Just,
      x' == y' =
    error $ show x ++ " has same position effect as " ++ show y

applyLot x y' =
    -- trace ("x^.symbol   = " ++ show (x^.Ledger.symbol)) $
    -- trace ("x^.quantity = " ++ show (x^.quantity)) $
    -- trace ("x^.refs     = " ++ show (x^.refs)) $
    -- trace ("y^.quantity = " ++ show (y^.quantity)) $
    -- trace ("y^.refs     = " ++ show (y^.refs)) $
    -- trace ("xcst        = " ++ show xcst) $
    -- trace ("ycst        = " ++ show ycst) $
    -- trace ("xc          = " ++ show xc) $
    -- trace ("yc          = " ++ show yc) $
    -- trace ("xps         = " ++ show xps) $
    -- trace ("yps         = " ++ show yps) $
    -- trace ("xq          = " ++ show xq) $
    -- trace ("yq          = " ++ show yq) $
    -- trace ("n           = " ++ show n) $
    -- trace ("n * yps     = " ++ show (n * yps)) $
    -- trace ("gain        = " ++ show gain) $
    LotApplied {..}
  where
    sign l | l^.quantity < 0 = negate
           | otherwise = id

    y | isTransactionSubType OptionExpiration y' = y' & quantity %~ negate
      | otherwise = y'

    xcst = lotCost x
    ycst = lotCost y
    xc   = sign x xcst
    yc   = sign y ycst
    xps  = xcst / x^.quantity
    yps  = ycst / y^.quantity
    xq   = abs (x^.quantity)
    yq   = abs (y^.quantity)
    n    = min xq yq
    xn   = sign x n
    yn   = sign y n

    _gain :: Amount 2
    _gain | isTransactionSubType TransferOfSecurityOrOptionIn y = 0
          | otherwise =
            coerce (normalizeAmount mpfr_RNDN (coerce go :: Amount 3))
      where
        go | xq < yq   = n * yps + xc
           | xq > yq   = yc + n * xps
           | otherwise = yc + xc

    _used = Just $
        x & quantity     .~ (- xn)
          & Ledger.cost  ?~ n * abs xps
          & Ledger.price .~ y^.Ledger.price

    _kept | n < xq =
            Just $ x & quantity    -~ xn
                     & Ledger.cost ?~ (xq - n) * abs xps
          | otherwise = Nothing

    _left | n < yq =
            Just $ y & quantity    -~ yn
                     & Ledger.cost ?~ (yq - n) * abs yps
          | otherwise = Nothing

-- Handling fees is a touch tricky, since if we end up closing multiple
-- positions via a single sale or purchase, the fees are applied across all of
-- them. Yet if the fee is oddly divided, we must carry the remaining penny,
-- since otherwise .03 divided by 2 (for example) will round to two instances
-- of .01. See tests for examples.
handleFees :: Amount 2 -> [LotAndPL t] -> [LotAndPL t]

-- If there is only a single transaction to apply the fee to, we add (or
-- subtract) it directly to (from) the basis cost, rather than dividing it
-- into the P/L of multiple transactions.
handleFees fee [LotAndPL 0.0 x] =
    [LotAndPL 0.0 $ x & Ledger.cost.mapped +~
         coerce (if x^.quantity < 0 then (-fee) else fee)]

handleFees fee lots = go True lots
  where
    go _ [] = []
    go b ((LotAndPL g x):xs) =
        LotAndPL
          (normalizeAmount mpfr_RNDNA g +
           normalizeAmount mpfr_RNDZ (sumOfParts x + if b then diff else 0))
          x : go False xs
      where
        diff = fee - sum (map (sumOfParts . view lot) lots)

        sumOfParts l =
            normalizeAmount mpfr_RNDZ (coerce (l^.quantity * perShare))
          where
            perShare = coerce fee / shares
            shares   = sum (map (^.lot.quantity) lots)
