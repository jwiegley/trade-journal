{-# LANGUAGE DataKinds #-}

module Journal.Model where

import Control.Applicative
import Control.Arrow
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List (tails)
import Journal.Amount
import Journal.Split
import Journal.Types
import Journal.Utils
import Prelude hiding (Double, Float)

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processActions :: [Action] -> ([StateChange], [Lot])
processActions =
  unzipBoth
    . (`evalState` [])
    . mapM (\x -> first (Action x :) <$> processAction x)

processAction :: Action -> State [Lot] ([StateChange], [Lot])
processAction action = do
  changes <- impliedChanges action
  unzipBoth <$> mapM go (Clear : changes)
  where
    go chg = case chg of
      Action _ -> error "Action is not produced by impliedChanges"
      Result e -> pure ([chg], [e])
      Submit a -> first (\xs -> chg : xs ++ [SubmitEnd]) <$> processAction a
      Clear -> ([chg], []) <$ put []
      SubmitEnd -> pure ([chg], [])
      ConsEvent e -> ([chg], []) <$ modify (e :)
      SnocEvent e -> ([chg], []) <$ modify (<> [e])

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

handle :: [Lot] -> Action -> Writer [StateChange] (Maybe Action)
handle (open : _) act
  | BuySell close <- act,
    any (== Position Open) (open ^. details),
    (open ^. amount > 0) == (close ^. amount < 0) =
    closePosition open close
handle (open : _) act
  | Wash wash <- act,
    any (== Position Open) (open ^. details),
    not (any (== PartsWashed) (open ^. details)),
    (open ^. amount > 0) == (wash ^. amount < 0),
    (open ^. time) `distance` (wash ^. time) <= 30 =
    washExistingPosition open wash
handle (close : _) act
  | BuySell open <- act,
    Just adj <- close ^? details . traverse . _WashSaleAdjust,
    (open ^. amount > 0) == (close ^. amount < 0),
    (close ^. time) `distance` (open ^. time) <= 30 =
    washNewPosition close open adj
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] (BuySell x) = do
  let y = x & details <>~ [Position Open]
  tell [SnocEvent y, Result y]
  pure Nothing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] (Wash x) = do
  let loss = sum (x ^.. details . traverse . gainLoss)
  assert (loss < 0) $ do
    tell [SnocEvent (x & details .~ [WashSaleAdjust loss])]
    pure Nothing
-- If none of the above apply, then nothing is done for this element of the
-- history.
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
-- to close an existing one for a profit or loss. If there is a loss within 30
-- days of the opening, it implies a wash sale adjustment of any preceeding or
-- subsequent openings 30 days before or after.
closePosition :: Lot -> Lot -> Writer [StateChange] (Maybe Action)
closePosition open close = do
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
    -- After closing at a loss, and if the loss occurs within 30 days
    -- of its corresponding open, and there is another open within 30
    -- days of the loss, close it and re-open so it's repriced by the
    -- wash loss.
    if (close ^. time) `distance` (open ^. time) <= 30 && pl < 0
      then do
        tell [Submit (Wash res)]
        forM_ (s ^? _SplitKept) $ \k ->
          tell [SnocEvent (k & details <>~ [PartsWashed])]
      else forM_ (s ^? _SplitKept) $ \k ->
        tell [SnocEvent k]
  pure $ BuySell <$> (d ^? _SplitKept)

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washExistingPosition :: Lot -> Lot -> Writer [StateChange] (Maybe Action)
washExistingPosition open wash = assert (wash ^. amount /= 0) $ do
  let (s, d) = open `alignLots` wash
      adj = sum (d ^.. _SplitUsed . details . traverse . washSaleAdjust)
  -- Wash failing closes by adding the amount to the cost basis of the
  -- opening transaction.
  tell $
    (SnocEvent <$> s ^.. _SplitKept)
      ++ ( SnocEvent . (details <>~ [WashSaleAdjust adj])
             <$> s ^.. _SplitUsed
         )
      ++ ( Result
             . ( \x ->
                   x & amount %~ negate
                     & details <>~ [ToBeWashed]
                     & details . traverse . _Position .~ Close
               )
             <$> s
             ^.. _SplitUsed
         )
      ++ ( Result . (details <>~ [WashSaleAdjust adj])
             <$> s ^.. _SplitUsed
         )
  pure $ Wash <$> d ^? _SplitKept

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washNewPosition :: Lot -> Lot -> Amount 6 -> Writer [StateChange] (Maybe Action)
washNewPosition close open adj = do
  let (s, d) = close `alignLots` open
  -- We wash failing closes by adding the amount to the cost basis of
  -- the opening transaction. Thus, we generate three instances of
  -- WashLossApplied, but only one OpenPosition.
  tell $
    (SnocEvent <$> s ^.. _SplitKept)
      ++ ( SnocEvent . (details <>~ [Position Open, WashSaleAdjust adj])
             <$> d ^.. _SplitUsed
         )
      ++ ( Result . (details <>~ [Position Open, WashSaleAdjust adj])
             <$> d ^.. _SplitUsed
         )
  pure $ BuySell <$> d ^? _SplitKept
