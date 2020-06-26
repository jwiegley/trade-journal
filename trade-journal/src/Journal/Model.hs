{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Model where

import Control.Applicative
import Control.Arrow
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (sortOn)
import Data.List (tails)
import Journal.Amount
import Journal.Split
import Journal.Types
import Journal.Utils
import Prelude hiding (Double, Float)

data JournalState = JournalState
  { _nextId :: Int,
    _entries :: IntMap Lot
  }

makeLenses ''JournalState

newJournalState :: JournalState
newJournalState = JournalState 1 IM.empty

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processActions :: [Action Lot] -> ([StateChange Lot], [Lot])
processActions =
  unzipBoth
    . (`evalState` newJournalState)
    . mapM (\x -> first (Action x :) <$> processAction x)

processAction :: Action Lot -> State JournalState ([StateChange Lot], [Lot])
processAction action = do
  changes <- impliedChanges action
  unzipBoth <$> mapM go changes
  where
    go chg = case chg of
      Action _ -> error "Action is not produced by impliedChanges"
      Result e -> pure ([chg], [e])
      Submit a -> first (\xs -> chg : xs ++ [SubmitEnd]) <$> processAction a
      SubmitEnd -> pure ([chg], [])
      AddEntry e -> do
        ident <- use nextId
        nextId += 1
        entries . at ident ?= e
        pure ([chg], [])
      RemoveEntry ident -> do
        entries . at ident .= Nothing
        pure ([chg], [])
      ReplaceEntry ident e -> do
        entries . at ident ?= e
        pure ([chg], [])

impliedChanges :: Action Lot -> State JournalState [StateChange Lot]
impliedChanges x = do
  hist <- gets (sortOn fst . toListOf (entries . ifolded . withIndex))
  let (mx, changes) = runWriter $ (\f -> foldM f (Just x) (tails hist)) $
        \ml hs -> case ml of
          Nothing -> pure Nothing
          Just x' -> handle hs x'
  case mx of
    Just _ -> error $ "impliedChanges: unexpected remainder: " ++ show mx
    Nothing -> pure changes

handle :: [(Int, Lot)] -> Action Lot -> Writer [StateChange Lot] (Maybe (Action Lot))
handle ((n, open) : _) act
  | BuySell close <- act,
    any (== Position Open) (open ^. details),
    (open ^. amount > 0) == (close ^. amount < 0) =
    closePosition n open close
handle ((n, open) : _) act
  | Wash wash <- act,
    any (== Position Open) (open ^. details),
    not (any (== PartsWashed) (open ^. details)),
    (open ^. amount > 0) == (wash ^. amount < 0),
    (open ^. time) `distance` (wash ^. time) <= 30 =
    washExistingPosition n open wash
handle ((n, close) : _) act
  | BuySell open <- act,
    Just adj <- close ^? details . traverse . _WashSaleAdjust . _2,
    (open ^. amount > 0) == (close ^. amount < 0),
    (close ^. time) `distance` (open ^. time) <= 30 =
    washNewPosition n close open adj
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] (BuySell x) = do
  let y = x & details <>~ [Position Open]
  tell [AddEntry y, Result y]
  pure Nothing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] (Wash x) = do
  let loss = sum (x ^.. details . traverse . gainLoss)
  assert (loss < 0) $ do
    tell [AddEntry (x & details .~ [WashSaleAdjust WashPending loss])]
    pure Nothing
-- If none of the above apply, then nothing is done for this element of the
-- history.
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
-- to close an existing one for a profit or loss. If there is a loss within 30
-- days of the opening, it implies a wash sale adjustment of any preceeding or
-- subsequent openings 30 days before or after.
closePosition :: Int -> Lot -> Lot -> Writer [StateChange Lot] (Maybe (Action Lot))
closePosition n open close = do
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
    let mayWash = (close ^. time) `distance` (open ^. time) <= 30 && pl < 0
    tell
      [ case s ^? _SplitKept of
          Just k ->
            ReplaceEntry
              n
              ( if mayWash
                  then k & details <>~ [PartsWashed]
                  else k
              )
          Nothing -> RemoveEntry n
      ]
    when mayWash $ do
      tell [Submit (Wash res)]
  pure $ BuySell <$> (d ^? _SplitKept)

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washExistingPosition ::
  Int ->
  Lot ->
  Lot ->
  Writer [StateChange Lot] (Maybe (Action Lot))
washExistingPosition n open wash = assert (wash ^. amount /= 0) $ do
  let (s, d) = open `alignLots` wash
      adj = sum (d ^.. _SplitUsed . details . traverse . washSaleAdjust)
  -- Wash failing closes by adding the amount to the cost basis of the
  -- opening transaction.
  tell $
    [ case s ^? _SplitKept of
        Just e -> ReplaceEntry n e
        Nothing -> RemoveEntry n
    ]
      ++ ( AddEntry . (details <>~ [WashSaleAdjust WashRetroactive adj])
             <$> s ^.. _SplitUsed
         )
  pure $ Wash <$> d ^? _SplitKept

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washNewPosition ::
  Int ->
  Lot ->
  Lot ->
  Amount 6 ->
  Writer [StateChange Lot] (Maybe (Action Lot))
washNewPosition n close open adj = do
  let (s, d) = close `alignLots` open
  -- We wash failing closes by adding the amount to the cost basis of
  -- the opening transaction. Thus, we generate three instances of
  -- WashLossApplied, but only one OpenPosition.
  tell $
    [ case s ^? _SplitKept of
        Just e -> ReplaceEntry n e
        Nothing -> RemoveEntry n
    ]
      ++ ( AddEntry
             . ( details
                   <>~ [ Position Open,
                         WashSaleAdjust WashOnOpen adj
                       ]
               )
             <$> d ^.. _SplitUsed
         )
      ++ ( Result
             . ( details
                   <>~ [ Position Open,
                         WashSaleAdjust WashOnOpen adj
                       ]
               )
             <$> d ^.. _SplitUsed
         )
  pure $ BuySell <$> d ^? _SplitKept
