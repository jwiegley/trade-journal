{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module ThinkOrSwim.Gains where

import Control.Lens
import Control.Monad.State
import Data.Ledger as Ledger
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types
-- import Debug.Trace

-- The function replicates the logic used by GainsKeeper to determine what
-- impact a given transaction, based on existing positions, should have on an
-- account.
gainsKeeper :: API.Transaction -> CommodityLot
            -> State OpenTransactions [(Double, CommodityLot)]
gainsKeeper t lot = do
    let sym   = lot^.Ledger.symbol
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        cst   = abs (t^.item.API.cost - fees')

    use (at sym) >>= \case
        -- If there are no existing lots, then this is either a purchase or a
        -- short sale.
        Nothing -> do
            let l = lot' cst
            let new = if l^.quantity /= 0 then [l] else []
            at sym .= case new of [] -> Nothing; xs -> Just xs
            -- traceM ("new: " ++ show new)
            pure $ (0.0,) <$> new

        -- If there are existing lots for this symbol, then if the current
        -- would add to or deduct from those positions, then it closes as much
        -- of that previous positions as quantities dictate.
        Just ls -> do
            let l = lot' cst
                -- We wrap all the entry in Just, with a Nothing at the end,
                -- so that we know to wrap up the fold.
                (_, res, new) = foldr fifo (l, [], [])
                    (reverse (Nothing : map Just (reverse ls)))
            at sym .= case new of [] -> Nothing; xs -> Just xs
            -- traceM ("existing: " ++ show ls)
            -- traceM ("res: " ++ show res)
            pure res
  where
    lot' cst = lot
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId)]

    fifo :: Maybe CommodityLot
         -> (CommodityLot, [(Double, CommodityLot)], [CommodityLot])
         -> (CommodityLot, [(Double, CommodityLot)], [CommodityLot])
    fifo (Just x) (l, res, rest) = (l, res, x:rest)
    fifo Nothing (l, res, rest)  = (l, (0.0, l):res, rest)
