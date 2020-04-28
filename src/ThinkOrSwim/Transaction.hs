{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module ThinkOrSwim.Transaction
    ( Transactional(..)
    , alignLots
    , splitLots
    , sumTransactions
    , renderConsidered
    , applyTo
    , renderTransactions
    , showTransactions
    ) where

import Control.Lens
import Data.Amount
import Data.Coerce
import Data.List (foldl')
import Data.Split
import Data.Text (Text)
import Data.Time
import Data.Utils (renderList)
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint

class Transactional t where
    symbol   :: Lens' t Text
    quantity :: Lens' t (Amount 4)
    cost     :: Lens' t (Amount 4)
    price    :: Lens' t (Amount 4)
    day      :: Lens' t Day
    loss     :: Lens' t (Amount 2)

    washDeferred :: Lens' t (Maybe (Amount 2))
    washEligible :: Lens' t Bool

    -- Given a loss-bearing transaction, wash the loss by transferring it to
    -- the cost basis of the second transaction. The result is the updated
    -- version of both transactions, the first with loss removed, the second
    -- with wash loss applied.
    washLoss :: Bool -> t -> t -> t

    -- If this opening transaction has a wash loss applied, unwash it so it
    -- can be recorded in the history of events that may affect the
    -- disposition of future losses.
    clearLoss :: t -> t

    -- True if this transaction is a transfer from previous books.
    isTransferIn :: t -> Bool

    -- True if this pair are opening and closing transactions, or closing and
    -- opening.
    arePaired :: t -> t -> Bool

    -- True if the second transaction represents an instrument materially
    -- equivalent to the first.
    areEquivalent :: t -> t -> Bool

    showPretty :: t -> String

alignLots :: (Transactional a, Transactional b)
          => a -> b -> (Split a, Split b)
alignLots x y
    | xq == 0 && yq == 0 = ( None x, None y )
    | xq == 0           = ( None x, All  y )
    | yq == 0           = ( All  x, None y )
    | abs xq == abs yq  = ( All  x, All  y )
    | abs xq <  abs yq  =
        ( All x
        , Some (y & quantity .~ sign yq xq
                  & cost     .~ abs xq * ycps
                  & loss     .~ coerce (abs xq) * ylps)
               (y & quantity .~ sign yq diff
                  & cost     .~ diff * ycps
                  & loss     .~ coerce diff * ylps)
        )
    | otherwise =
        ( Some (x & quantity .~ sign xq yq
                  & cost     .~ abs yq * xcps
                  & loss     .~ coerce (abs yq) * xlps)
               (x & quantity .~ sign xq diff
                  & cost     .~ diff * xcps
                  & loss     .~ coerce diff * xlps)
        , All y
        )
  where
    xq   = x^.quantity
    yq   = y^.quantity
    xcps = x^.cost / abs xq
    ycps = y^.cost / abs yq
    xlps = x^.loss / coerce (abs xq)
    ylps = y^.loss / coerce (abs yq)
    diff = abs (abs xq - abs yq)

splitLots :: Transactional a => a -> a -> Applied () a a
splitLots x y = uncurry splits (x `alignLots` y)

applyTo :: Transactional a => Traversal' a (Amount n) -> (Amount m, a) -> a
applyTo l (n, x) = x & l +~ coerce n

-- The idea of this function is to replicate what Ledger will calculate the
-- sum to be, so that if there's any discrepancy we can add a rounding
-- adjustment to pass the balancing check.
sumTransactions :: Transactional a => [a] -> Amount 2
sumTransactions = foldl' go 0
  where
    norm      = normalizeAmount mpfr_RNDNA
    cst l     = sign (l^.quantity) (l^.cost)
    go acc pl = acc + norm (coerce (cst pl)) + norm (pl^.loss)

renderTransactions :: Transactional a => [a] -> Doc
renderTransactions = renderList (text . showPretty)

showTransactions :: Transactional a => [a] -> String
showTransactions = show . renderTransactions

renderConsidered :: Transactional a => Considered a a a -> Doc
renderConsidered c =
        text "c^.fromList    "
        <> renderTransactions (c^.fromList)
    $$ text "c^.newList     "
        <> renderTransactions (c^.newList)
    $$ text "c^.fromElement "
        <> renderTransactions (c^.fromElement)
    $$ text "c^.newElement  "
        <> text (show (fmap showPretty (c^.newElement)))
