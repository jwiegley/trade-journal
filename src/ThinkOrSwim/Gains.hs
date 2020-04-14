{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Foldable (foldl')
import Data.Ledger as Ledger
import Data.Maybe (isJust, maybeToList)
import GHC.TypeLits
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

-- import Data.List (intercalate)
-- import Data.Text (unpack)
-- import Debug.Trace

nog :: (Functor f, KnownNat n) => f a -> f (Amount n, a)
nog = fmap (0,)

-- The function seeks to replicate the logic used by GainsKeeper to determine
-- what impact a given transaction, based on existing positions, should have
-- on an account.
gainsKeeper :: API.Transaction -> CommodityLot API.Transaction
            -> State (OpenTransactions API.Transaction)
                    [(Amount 2, CommodityLot API.Transaction)]
gainsKeeper t lot = do
    let sym   = lot^.Ledger.symbol
        cst   = abs (t^.item.API.cost)
        fees' = t^.fees_.regFee + t^.fees_.otherCharges + t^.fees_.commission

    use (at sym) >>= \case
        -- If there are no existing lots, then this is either a purchase or a
        -- short sale.
        Nothing -> do
            let l = lot' (coerce cst)
                ls = if l^.quantity /= 0 then [l] else []
                keep = handleFees fees' (nog ls)
            -- traceM (unpack sym ++ ": ==> ["
            --         ++ intercalate "," (map (showCommodityLot . snd) keep)
            --         ++ "]")
            at sym .= case keep of [] -> Nothing; xs -> Just (map snd xs)
            pure keep

        -- If there are existing lots for this symbol, then if the current
        -- would add to or deduct from those positions, then it closes as much
        -- of that previous positions as quantities dictate.
        Just ls -> do
            let (res, keep, new) = calculateGains (lot' (coerce cst)) ls
                res' = handleFees fees' (res ++ maybeToList (nog new))
                next = case keep ++ if isJust new
                                   then [snd (last res')]
                                   else [] of
                           [] -> Nothing
                           xs -> Just xs
                -- show' = map (\(x, y) -> "("  ++ show x
                --                     ++ ", " ++ showCommodityLot y ++ ")")
            -- traceM (unpack sym ++ ": "
            --         ++ showCommodityLot (lot' (coerce cst))
            --         ++ "\n  --> ["
            --         ++ intercalate "," (map showCommodityLot ls)
            --         ++ "]"
            --         ++ "\n  <-- ["
            --         ++ intercalate ", " (show' res')
            --         ++ "]"
            --         ++ "\n  ==> ["
            --         ++ intercalate "," (maybe [""] (map showCommodityLot) next)
            --         ++ "]")
            at sym .= next
            pure res'
  where
    lot' cst = lot
        & Ledger.cost  .~ (if cst /= 0 then Just cst else Nothing)
        & purchaseDate ?~ t^.xactDate
        & refs         .~ [Ref OpeningOrder (t^.xactId) t]

-- Handling fees is a touch tricky, since if we end up closing multiple
-- positions via a single sale or purchase, the fees must be applied evenly
-- across all of them. Yet if the fee is odd, we must carry the remaining
-- penny, since otherwise .03 divided by 2 (for example) will round to two
-- instances of .01. See tests for examples.
handleFees :: Amount 2 -> [(Amount 2, CommodityLot t)]
           -> [(Amount 2, CommodityLot t)]
handleFees _ [] = []
handleFees fee [(0.0, x)]
    | x^.quantity < 0 = [(0.0, x & Ledger.cost.mapped -~ coerce fee)]
    | otherwise       = [(0.0, x & Ledger.cost.mapped +~ coerce fee)]
handleFees fee lots = go True lots
  where
    shares   = sum (map (^._2.quantity) lots)
    perShare = coerce fee / shares

    go :: Bool -> [(Amount 2, CommodityLot t)] -> [(Amount 2, CommodityLot t)]
    go _ [] = []
    go b ((g, x):xs) =
        ( normalizeAmount mpfr_RNDNA g +
          normalizeAmount mpfr_RNDZ (sumOfParts x + if b then diff else 0)
        , x
        ) : go False xs
      where
        diff = fee - sum (map (sumOfParts . snd) lots)

    sumOfParts :: CommodityLot t -> Amount 2
    sumOfParts l = normalizeAmount mpfr_RNDZ (coerce (l^.quantity * perShare))

(@@) :: Amount 4 -> Amount 4 -> CommodityLot t
q @@ c = newCommodityLot & quantity .~ q & Ledger.cost ?~ c

data LotApplied t = LotApplied
    { gain :: Amount 2
    , used :: Maybe (CommodityLot t) -- the portion subtracted out
    , kept :: Maybe (CommodityLot t) -- what remains
    , left :: Maybe (CommodityLot t) -- the portion unsubtracted
    }
    deriving (Eq, Ord, Show)

isTransactionSubType :: TransactionSubType -> CommodityLot API.Transaction
                     -> Bool
isTransactionSubType subty y = case y^.refs of
    [r] | r^.refOrig.transactionInfo_.transactionSubType == subty -> True
    _ -> False

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

    gain :: Amount 2
    gain | isTransactionSubType TransferOfSecurityOrOptionIn y = 0
         | otherwise =
           coerce (normalizeAmount mpfr_RNDN (coerce go :: Amount 3))
      where
        go | xq < yq   = n * yps + xc
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

calculateGains
    :: CommodityLot API.Transaction -> [CommodityLot API.Transaction]
    -> ([(Amount 2, CommodityLot API.Transaction)],
       [CommodityLot API.Transaction], Maybe (CommodityLot API.Transaction))
calculateGains l ls =
    let (x, xs, ys) = foldl' fifo (Just l, [], []) ls
    in (reverse xs, reverse ys, x)
  where
    fifo (Nothing, res, keep) x = (Nothing, res, x:keep)
    fifo (Just z, res, keep) x =
        ( left
        , maybe res ((:res) . (gain,)) used
        , maybe keep (:keep) kept
        )
      where
        LotApplied {..} = x `applyLot` z
