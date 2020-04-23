{-# LANGUAGE DataKinds #-}

module ThinkOrSwim.Transaction
    ( Transactional(..)
    , alignLots
    , sumTransactions
    ) where

import Control.Lens
import Data.Amount
import Data.Coerce
import Data.List (foldl')
import Data.Split
import Data.Time
import Prelude hiding (Float, Double)

class Transactional t where
    quantity :: Lens' t (Amount 4)
    cost     :: Lens' t (Amount 4)
    price    :: Lens' t (Amount 4)
    day      :: Lens' t Day
    loss     :: Lens' t (Amount 2)

    -- Given a loss-bearing transaction, wash the loss by transferring it to
    -- the cost basis of the second transaction. The result is the updated
    -- version of both transactions, the first with loss removed, the second
    -- with wash loss applied.
    washLoss :: t -> t -> (t, t)

    -- If this opening transaction has a wash loss applied, unwash it so it
    -- can be recorded in the history of events that may affect the
    -- disposition of future losses.
    unwash :: t -> t

    -- True if this transaction is merely a transfer in from previous books.
    isTransferIn :: t -> Bool

    -- True if this pair are opening and closing transactions, or closing and
    -- opening.
    arePaired :: t -> t -> Bool

    -- True if the second transaction represents an instrument materially
    -- equivalent to the first.
    areEquivalent :: t -> t -> Bool

    align :: t -> t -> (Split t, Split t)

    showPretty :: t -> String

alignLots :: Transactional a => a -> a -> (Split a, Split a)
alignLots x y
    | xq == 0 && yq == 0 = ( None x, None y )
    | xq == 0           = ( None x, All  y )
    | yq == 0           = ( All  x, None y )
    | abs xq == abs yq  = ( All  x, All  y )
    | abs xq <  abs yq  =
        ( All x
        , Some (y & quantity .~ sign yq xq
                  & cost     .~ abs xq * yps)
               (y & quantity .~ sign yq diff
                  & cost     .~ diff * yps)
        )
    | otherwise =
        ( Some (x & quantity .~ sign xq yq
                  & cost     .~ abs yq * xps)
               (x & quantity .~ sign xq diff
                  & cost     .~ diff * xps)
        , All y
        )
  where
    xq    = x^.quantity
    yq    = y^.quantity
    xcst  = x^.cost
    ycst  = y^.cost
    xps   = xcst / abs xq
    yps   = ycst / abs yq
    diff  = abs (abs xq - abs yq)

-- The idea of this function is to replicate what Ledger will calculate the
-- sum to be, so that if there's any discrepancy we can add a rounding
-- adjustment to pass the balancing check.
sumTransactions :: Transactional a => [a] -> Amount 2
sumTransactions = foldl' go 0
  where
    norm      = normalizeAmount mpfr_RNDNA
    cst l     = sign (l^.quantity) (l^.cost)
    go acc pl = acc + norm (coerce (cst pl)) + norm (pl^.loss)
