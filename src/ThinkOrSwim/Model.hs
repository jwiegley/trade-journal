{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# LANGUAGE FunctionalDependencies #-}

-- {-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RecordWildCards #-}

-- {-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE ViewPatterns #-}

module ThinkOrSwim.Model where

import Control.Applicative
import Control.Arrow
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Amount
import Data.Foldable
import Data.List (tails)
-- import Data.Map (Map)
import Data.Split
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Time
-- import Data.Utils
-- import Text.PrettyPrint (Doc)
-- import qualified Text.PrettyPrint as P
-- import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Effect = Open | Close
  deriving (Show, Eq, Ord, Enum, Bounded)

data Annotation
  = Fees (Amount 6)
  | Commission (Amount 6)
  | GainLoss (Amount 6)
  | WashSaleAdjust (Amount 6)
  | PartsWashed
  | Position Effect
  deriving (Show, Eq, Ord)

makePrisms ''Annotation

gainLoss :: Traversal' Annotation (Amount 6)
gainLoss f = \case
  GainLoss pl -> GainLoss <$> f pl
  s -> pure s

fees :: [Annotation] -> Amount 6
fees [] = 0
fees (Fees x : xs) = x + fees xs
fees (Commission x : xs) = x + fees xs
fees (_ : xs) = fees xs

-- | A 'Lot' represents a collection of shares, with a given price and a
--   transaction date.
data Lot
  = Lot
      { _amount :: Amount 6, -- positive is long, negative is short
        _price :: Amount 6,
        _time :: UTCTime,
        -- | All details are expressed "per share", just like the price.
        _details :: [Annotation]
      }
  deriving (Show, Eq, Ord)

makeLenses ''Lot

data Action
  = -- | The sign of the Lot 'amount' indicates buy or sell.
    BuySell Lot
  | Wash Lot
  deriving (Show, Eq, Ord)

makePrisms ''Action

data StateChange
  = Submit Action
  | SubmitEnd
  | Result Lot
  | ConsEvent Lot
  | SnocEvent Lot
  deriving (Show, Eq, Ord)

makePrisms ''StateChange

alignLots :: Lot -> Lot -> (Split Lot, Split Lot)
alignLots x y =
  let (s, d) = align amount amount (x & amount %~ abs) (y & amount %~ abs)
   in ( if negX
          then s & _Splits . amount %~ negate
          else s,
        if negY
          then d & _Splits . amount %~ negate
          else d
      )
  where
    negX = x ^. amount < 0
    negY = y ^. amount < 0

distance :: UTCTime -> UTCTime -> Integer
distance x y = abs (utctDay x `diffDays` utctDay y)

handle :: [Lot] -> Action -> Writer [StateChange] (Maybe Action)
handle (open : _) act
  | any (== Position Open) (open ^. details),
    BuySell close <- act,
    (open ^. amount > 0) == (close ^. amount < 0) = do
    let (s, d) = open `alignLots` close
    forM_ ((,) <$> s ^? _SplitUsed <*> d ^? _SplitUsed) $ \(su, du) -> do
      let pl =
            ( if open ^. amount > 0
                then du ^. price - su ^. price
                else su ^. price - du ^. price
            )
              - fees (du ^. details)
          res = du & details <>~ [GainLoss pl]
      tell [Result res]
      forM_ (s ^? _SplitKept) $ \k ->
        tell [SnocEvent (k & details <>~ [PartsWashed])]
      -- After closing at a loss, and if the loss occurs within 30 days
      -- of its corresponding open, and there is another open within 30
      -- days of the loss, close it and re-open so it's repriced by the
      -- wash loss.
      when ((close ^. time) `distance` (open ^. time) <= 30 && pl < 0) $
        tell [Submit (Wash res)]
    pure $ BuySell <$> (d ^? _SplitKept)
handle [] (BuySell x) = do
  let y = x & details <>~ [Position Open]
  tell [SnocEvent y, Result y]
  pure Nothing
handle [] (Wash x) = do
  let loss = sum (x ^.. details . traverse . gainLoss)
  assert (loss < 0) $ do
    tell [SnocEvent (x & details .~ [WashSaleAdjust loss])]
    pure Nothing
handle _ x = pure $ Just x

impliedChanges :: Action -> State [Lot] [StateChange]
impliedChanges x = do
  hist <- get
  let (mx, changes) = runWriter $ (\f -> foldM f (Just x) (tails hist)) $
        \ml hs -> case ml of
          Nothing -> Nothing <$ case hs of
            [] -> pure ()
            (h : _) -> tell [SnocEvent h]
          Just x' -> handle hs x'
  case mx of
    Just _ -> error $ "impliedChanges: unexpected remainder: " ++ show mx
    Nothing -> pure changes

unzipBoth :: [([a], [b])] -> ([a], [b])
unzipBoth = (concat *** concat) . unzip

processLot :: Action -> State [Lot] ([StateChange], [Lot])
processLot action = do
  changes <- impliedChanges action
  unzipBoth <$> mapM go changes
  where
    go chg = case chg of
      Result e -> pure ([chg], [e])
      Submit a -> first (\xs -> chg : xs ++ [SubmitEnd]) <$> processLot a
      SubmitEnd -> pure ([chg], [])
      ConsEvent e -> ([chg], []) <$ modify (e :)
      SnocEvent e -> ([chg], []) <$ modify (<> [e])

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processLots :: [Action] -> ([StateChange], [Lot])
processLots = unzipBoth . (`evalState` []) . mapM processLot