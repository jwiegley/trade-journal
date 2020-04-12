{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module ThinkOrSwim.Gains where

import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Coerce
import Data.Foldable (foldl')
import Data.Ledger as Ledger
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

-- import Data.List (intercalate)
-- import Data.Text (unpack)
-- import Debug.Trace

-- The function seeks to replicate the logic used by GainsKeeper to determine
-- what impact a given transaction, based on existing positions, should have
-- on an account.
gainsKeeper :: API.Transaction -> CommodityLot
            -> State OpenTransactions [(Amount 6, CommodityLot)]
gainsKeeper t lot = do
    let sym   = lot^.Ledger.symbol
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission
        cst   = abs (t^.item.API.cost - coerce fees')

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

data LotApplied = LotApplied
    { gain :: Amount 6
    , used :: Maybe CommodityLot -- the portion subtracted out
    , kept :: Maybe CommodityLot -- what remains
    , left :: Maybe CommodityLot -- the portion unsubtracted
    }
    deriving (Eq, Ord, Show)

-- Given some lot x, apply lot y. If x is positive, and y is negative, this is
-- a share sell; a buy for the reverse. If both have the same sign, nothing is
-- done. If the cost basis per share of the two are different, there will be a
-- gain (or less, if negative). Also, we need to return the part of 'x' that
-- remains to be further deducted from, and how much was consumed, and
-- similarly for 'y'.
applyLot :: CommodityLot -> CommodityLot -> LotApplied
applyLot x y
    |   x^.quantity < 0 && y^.quantity < 0
      || x^.quantity > 0 && y^.quantity > 0 =
    LotApplied 0.0 Nothing (Just x) (Just y)
applyLot x y =
    -- trace ("x^.symbol   = " ++ show (x^.Ledger.symbol))        $
    -- trace ("x^.quantity = " ++ show (x^.quantity))             $
    -- trace ("x^.refs     = " ++ show (x^.refs))                 $
    -- trace ("y^.quantity = " ++ show (y^.quantity))             $
    -- trace ("y^.refs     = " ++ show (y^.refs))                 $
    -- trace ("xcst        = " ++ show xcst)                      $
    -- trace ("ycst        = " ++ show ycst)                      $
    -- trace ("xps         = " ++ show xps)                       $
    -- trace ("yps         = " ++ show yps)                       $
    -- trace ("xq          = " ++ show xq)                        $
    -- trace ("yq          = " ++ show yq)                        $
    -- trace ("n           = " ++ show n)                         $
    -- trace ("gain        = " ++ show gain)                      $
    -- trace ("gain        = " ++ show (gain^.from (rounded @2))) $
    LotApplied {..}
  where
    sign l | l^.quantity < 0 = negate
           | otherwise = id

    xcst = lotCost x
    ycst = lotCost y
    xps  = xcst / x^.quantity
    yps  = ycst / y^.quantity
    xq   = abs (x^.quantity)
    yq   = abs (y^.quantity)
    n    = min xq yq
    xn   = sign x n
    yn   = sign y n
    xc   = sign x xcst
    yc   = sign y ycst

    gain :: Amount 6
    gain | xq < yq   = n * yps + xc
         | xq > yq   = yc + n * xps
         | otherwise = yc + xc

    used = Just $
        x & quantity     .~ (- xn)
          & Ledger.cost  ?~ n * abs xps
          & Ledger.price .~ y^.Ledger.price

    kept | n < xq =
           Just $ x & quantity    -~ xn
                    & Ledger.cost ?~ (xq - n) * abs xps
         | otherwise = Nothing

    left | n < yq =
           Just $ y & quantity    -~ yn
                    & Ledger.cost ?~ (yq - n) * abs yps
         | otherwise = Nothing

calculateGains :: CommodityLot -> [CommodityLot]
               -> ([(Amount 6, CommodityLot)], [CommodityLot])
calculateGains l ls =
    let (x, xs, ys) = foldl' fifo (Just l, [], []) ls
    in (maybe (reverse xs) (reverse . (:xs) . (0,)) x,
        maybe (reverse ys) (reverse . (:ys)) x)
  where
    fifo (Nothing, res, keep) x = (Nothing, res, x:keep)
    fifo (Just z, res, keep) x =
        ( left
        , maybe res ((:res) . (gain,)) used
        , maybe keep (:keep) kept
        )
      where
        LotApplied {..} = x `applyLot` z
