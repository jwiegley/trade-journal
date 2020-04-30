{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module ThinkOrSwim.Model where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Coerce
import           Data.Split
import           Data.Text (Text)
import           Data.Time
import           Data.Utils
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API

data Event t
    = OpenPosition t
    | ClosePosition t t
    | AdjustCostBasisForOpen t
    | AdjustCostBasis t t
    | RememberWashSaleLoss t
    | TransferEquitiesIn t
    | AssignOption t t
    | ExerciseOption t t
    | ExpireOption t
    deriving (Eq, Ord, Show)

class Transactional t where
    ident      :: Fold t API.TransactionId
    time       :: Fold t UTCTime
    kind       :: Fold t API.TransactionSubType
    cusip      :: Fold t Text
    symbol     :: Fold t Text
    underlying :: Fold t Text
    quantity   :: Lens' t (Amount 4)
    cost       :: Lens' t (Amount 4)
    fees       :: Fold t (Amount 2)
    refs       :: Lens' t [Event t]

data Lot t = Lot
    { _shares       :: Amount 4
    , _costOfShares :: Amount 4  -- the cost of a lot may be adjusted
    , _xact         :: t
    , _trail        :: [Event (Lot t)]
    }

makeLenses ''Lot

sliceOf :: (Transactional t, Functor f)
        => Lens' t (Amount 4) -> LensLike' f t (Amount n)
        -> LensLike' f t (Amount n)
sliceOf n l f t = t & l.percent (coerce (t^.n / t^.quantity)) %%~ f

sliceOf' :: (Transactional t, Functor f)
         => Amount 4 -> LensLike' f t (Amount n)
         -> LensLike' f t (Amount n)
sliceOf' n l f t = t & l.percent (coerce (n / t^.quantity)) %%~ f

instance Transactional t => Transactional (Lot t) where
    ident      = xact.ident
    time       = xact.time
    kind       = xact.kind
    cusip      = xact.cusip
    symbol     = xact.symbol
    underlying = xact.underlying
    quantity   = shares
    cost       = costOfShares
    fees       = shares `sliceOf` (xact.fees)
    refs       = trail

mkLot :: Transactional t => t -> Lot t
mkLot t = Lot
    { _shares       = t^.quantity
    , _costOfShares = t^.cost
    , _xact         = t
    , _trail        = []
    }

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
                  -- & loss     .~ coerce (abs xq) * ylps
               )
               (y & quantity .~ sign yq diff
                  & cost     .~ diff * ycps
                  -- & loss     .~ coerce diff * ylps
               )
        )
    | otherwise =
        ( Some (x & quantity .~ sign xq yq
                  & cost     .~ abs yq * xcps
                  -- & loss     .~ coerce (abs yq) * xlps
               )
               (x & quantity .~ sign xq diff
                  & cost     .~ diff * xcps
                  -- & loss     .~ coerce diff * xlps
               )
        , All y
        )
  where
    xq   = x^.quantity
    yq   = y^.quantity
    xcps = x^.cost / abs xq
    ycps = y^.cost / abs yq
    -- xlps = x^.loss / coerce (abs xq)
    -- ylps = y^.loss / coerce (abs yq)
    diff = abs (abs xq - abs yq)

loss :: Transactional t => Fold (Event t) (Amount 2)
loss f s = case s of
    OpenPosition _           -> pure s
    ClosePosition o c        -> s <$ f (coerce (o^.cost - c^.cost) - sumOf fees c)
    AdjustCostBasisForOpen _ -> pure s
    AdjustCostBasis _ _      -> pure s
    RememberWashSaleLoss _   -> pure s
    TransferEquitiesIn _     -> pure s
    AssignOption _ _         -> pure s
    ExerciseOption _ _       -> pure s
    ExpireOption _           -> pure s

longTerm :: Traversal' (Event t) Bool
longTerm = undefined

{-

Things that can happen which may change the raw sequence of transactions, or
affect the history of events having possible future implications:

- open position by buying/selling long/short
- close position with gain/loss by selling/covering, short/long-term/collectible
- adjust cost basis when opening position due to previous wash sale
- adjust cost basis of existing open position due to current wash sale
- remember fact of an unapplied wash sale loss
- equities transferred in
- shares assigned/carried away due to option assignment/exercise by counter-party
- shares bought/sold due to option exercise
- short/long-term loss/gain due to option expiration of long/short call/put

-}
