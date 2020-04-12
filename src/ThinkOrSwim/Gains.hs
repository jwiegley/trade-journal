{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module ThinkOrSwim.Gains where

import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Foldable (foldl')
import Data.Ledger as Ledger
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

-- import Data.List (intercalate)
-- import Data.Text (unpack)
-- import Debug.Trace

-- The function replicates the logic used by GainsKeeper to determine what
-- impact a given transaction, based on existing positions, should have on an
-- account.
gainsKeeper :: API.Transaction -> CommodityLot
            -> State OpenTransactions [(Amount 2, CommodityLot)]
gainsKeeper t lot = do
    let sym   = lot^.Ledger.symbol
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        cst   = abs (t^.item.API.cost - fees'^.from rounded)

    use (at sym) >>= \case
        -- If there are no existing lots, then this is either a purchase or a
        -- short sale.
        Nothing -> do
            let l = lot' cst
            let keep = if l^.quantity /= 0 then [l] else []
            -- traceM (unpack sym ++ ": // ["
            --         ++ intercalate "," (map showCommodityLot keep)
            --         ++ "]")
            at sym .= case keep of [] -> Nothing; xs -> Just xs
            pure $ (0,) <$> keep

        -- If there are existing lots for this symbol, then if the current
        -- would add to or deduct from those positions, then it closes as much
        -- of that previous positions as quantities dictate.
        Just ls -> do
            let (res, keep) = calculateGains (lot' cst) ls
            -- traceM (unpack sym ++ ": calculateGains ("
            --         ++ showCommodityLot (lot' cst)
            --         ++ ") ["
            --         ++ intercalate "," (map showCommodityLot ls)
            --         ++ "]\n"
            --         ++ "  ===> "
            --         ++ intercalate "," (map show res)
            --         ++ " // ["
            --         ++ intercalate "," (map showCommodityLot keep)
            --         ++ "]")
            at sym .= case keep of [] -> Nothing; xs -> Just xs
            pure res
  where
    lot' cst = lot
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId)]

(@@) :: Amount 6 -> Amount 6 -> CommodityLot
q @@ c = newCommodityLot
    & quantity .~ q
    & Ledger.cost ?~ c

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
applyLots :: CommodityLot -> CommodityLot
             -> ( Amount 2
               , ( Maybe CommodityLot -- the portion subtracted out
                 , Maybe CommodityLot -- what remains
                 )
               , Maybe CommodityLot   -- the portion unsubtracted
               )
applyLots x y
    |   x^.quantity < 0 && y^.quantity < 0
      || x^.quantity > 0 && y^.quantity > 0 =
    (0.0, (Nothing, Just x), Just y)
applyLots x y =
    -- trace ("x^.symbol   = " ++ show (x^.Ledger.symbol)) $
    -- trace ("x^.quantity = " ++ show (x^.quantity))      $
    -- trace ("x^.refs     = " ++ show (x^.refs))          $
    -- trace ("y^.quantity = " ++ show (y^.quantity))      $
    -- trace ("y^.refs     = " ++ show (y^.refs))          $
    let xcst = lotCost x^.from rounded
        ycst = lotCost y^.from rounded
        xps  = xcst / x^.quantity
        yps  = ycst / y^.quantity
        xq   = abs (x^.quantity)
        yq   = abs (y^.quantity)
        n    = min xq yq
        sign = \l -> if l^.quantity < 0 then negate else id
        xn   = sign x n
        yn   = sign y n
        xc   = sign x xcst
        yc   = sign y ycst
        gain = - (if | xq < yq   -> n * yps + xc
                     | xq > yq   -> yc + n * xps
                     | otherwise -> yc + xc)
    in -- trace ("xcst = " ++ show xcst) $
       -- trace ("ycst = " ++ show ycst) $
       -- trace ("xps  = " ++ show xps)  $
       -- trace ("yps  = " ++ show yps)  $
       -- trace ("xq   = " ++ show xq)   $
       -- trace ("yq   = " ++ show yq)   $
       -- trace ("n    = " ++ show n)    $
       -- trace ("gain = " ++ show gain) $
       -- trace ("gain = " ++ show (gain^.from (rounded @2))) $
       ( gain^.from rounded
       , ( Just $ x & quantity     .~ (- xn)
                    & Ledger.cost  ?~ n * abs xps
                    & Ledger.price .~ y^.Ledger.price
         , if n < xq
           then Just $ x & quantity    -~ xn
                         & Ledger.cost ?~ (xq - n) * abs xps
           else Nothing)
       , if n < yq
         then Just $ y & quantity    -~ yn
                       & Ledger.cost ?~ (yq - n) * abs yps
         else Nothing)

fifo :: (Maybe CommodityLot, [(Amount 2, CommodityLot)], [CommodityLot])
     -> CommodityLot
     -> (Maybe CommodityLot, [(Amount 2, CommodityLot)], [CommodityLot])
fifo (Nothing, res, keep) x = (Nothing, res, x:keep)
fifo (Just l, res, keep) x =
    let (gain, (used, kept), left) = x `applyLots` l
    in (left, maybe res ((:res) . (gain,)) used, maybe keep (:keep) kept)

calculateGains :: CommodityLot -> [CommodityLot]
               -> ([(Amount 2, CommodityLot)], [CommodityLot])
calculateGains l ls =
    let (x, xs, ys) = foldl' fifo (Just l, [], []) ls
        xs' = maybe (reverse xs) (reverse . (:xs) . (0,)) x
        ys' = maybe (reverse ys) (reverse . (:ys)) x
    in (xs', ys')
