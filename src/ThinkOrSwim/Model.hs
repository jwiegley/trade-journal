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
import           Data.Text (Text, unpack)
import           Data.Time
import           Data.Utils
import           Debug.Trace (traceM)
import           Prelude hiding (Float, Double, (<>))
import           Text.PrettyPrint
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Options (Options)
import qualified ThinkOrSwim.Options as Opts
import           ThinkOrSwim.Types
import           ThinkOrSwim.Wash (washSaleRule, openTransactions, symbolHistory)

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
    ident      :: Lens' t API.TransactionId
    time       :: Lens' t UTCTime
    kind       :: Lens' t API.TransactionSubType
    cusip      :: Lens' t Text
    symbol     :: Lens' t Text
    underlying :: Lens' t Text
    quantity   :: Lens' t (Amount 4)
    cost       :: Lens' t (Amount 4)
    fees       :: Lens' t (Amount 2)
    refs       :: Lens' t [Event t]

data Lot t = Lot
    { _shares :: Amount 4
    , _xact   :: t
    , _trail  :: [Event (Lot t)]
    }

makeLenses ''Lot

percent :: Num a => a -> Lens' a a
percent n f s = f (s * n) <&> \v -> v + (s - (s * n))

instance Transactional t => Transactional (Lot t) where
    ident      = xact.ident
    time       = xact.time
    kind       = xact.kind
    cusip      = xact.cusip
    symbol     = xact.symbol
    underlying = xact.underlying
    quantity   = shares
    cost f t   = f ((t^.xact.cost / t^.xact.quantity) * t^.shares) <&> \n ->
        t & xact.cost .~ n + (t^.xact.cost / t^.xact.quantity) *
                             (t^.xact.quantity - t^.shares)
    fees       = xact.fees
    refs       = trail

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
