{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Model where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Amount
import Data.Foldable
import Data.List (tails)
-- import Data.Map (Map)
-- import Data.Split
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Time
-- import Data.Utils
-- import Text.PrettyPrint as P
-- import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Effect = Open | Close
  deriving (Show, Eq, Ord, Enum, Bounded)

data Annotation
  = Fees (Amount 6)
  | Commission (Amount 6)
  | GainLoss (Amount 6)
  | WashSaleAdjust (Amount 6)
  deriving (Show, Eq, Ord)

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot
  = Lot
      { amount :: Amount 6, -- positive is a buy, negative a sell
        effect :: Effect,
        price :: Amount 6,
        time :: UTCTime,
        -- | All adjustments are expressed "per share", just like the price.
        details :: [Annotation]
      }
  deriving (Show, Eq, Ord)

data StateChange
  = Result
  | Submit
  | ConsEvent
  | SnocEvent
  deriving (Show, Eq, Ord, Enum, Bounded)

changesImplied ::
  ([Lot] -> Lot -> Writer [(StateChange, Lot)] (Maybe Lot)) ->
  Lot ->
  State [Lot] [(StateChange, Lot)]
changesImplied k lot = do
  hist <- get
  let (mlot, changes) = runWriter $ (\f -> foldM f (Just lot) (tails hist)) $
        \ml hs -> case ml of
          Nothing -> Nothing <$ case hs of
            [] -> pure ()
            (h : _) -> tell [(SnocEvent, h)]
          Just lot' -> k hs lot'
  case mlot of
    Just _ ->
      error $
        "changesImplied: unexpected remainder: "
          ++ show mlot
    Nothing -> pure changes

processLot :: Lot -> State [Lot] [Lot]
processLot = fmap concat . mapM go <=< changesImplied f
  where
    go (change, lot) = case change of
      Result -> pure [lot]
      Submit -> processLot lot
      ConsEvent -> [] <$ modify (lot :)
      SnocEvent -> [] <$ modify (<> [lot])
    f :: [Lot] -> Lot -> Writer [(StateChange, Lot)] (Maybe Lot)
    f (_ : _) _lot = undefined
    f [] lot | Close <- effect lot = do
      error $ "Cannot find open " ++ show lot
    f [] lot = do
      case effect lot of
        Open -> tell [(SnocEvent, lot)]
        _ -> pure ()
      tell [(Result, lot)]
      pure Nothing

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processLots :: [Lot] -> [Lot]
processLots = concat . (`evalState` []) . mapM processLot
