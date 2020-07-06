{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Model (processActions, processActionsWithChanges) where

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
import Journal.Split
import Journal.Types
import Journal.Utils
import Prelude hiding (Double, Float)

data JournalState = JournalState
  { _nextId :: Int,
    _events :: IntMap (Timed Event)
  }

makeLenses ''JournalState

newJournalState :: JournalState
newJournalState = JournalState 1 IM.empty

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processActions :: [Timed Action] -> [Timed Action]
processActions = snd . processActionsWithChanges

processActionsWithChanges :: [Timed Action] -> ([Change], [Timed Action])
processActionsWithChanges =
  unzipBoth
    . (`evalState` newJournalState)
    . mapM (\x -> first (SawAction x :) <$> processAction x)

processAction ::
  Timed Action ->
  State JournalState ([Change], [Timed Action])
processAction = fmap unzipBoth . mapM go <=< impliedChanges
  where
    go chg = case chg of
      SawAction _ -> error "Change not produced by impliedChanges"
      Result e -> pure ([chg], [e])
      Submit lot ->
        first (\xs -> chg : xs ++ [SubmitEnd])
          <$> processAction (Adjust <$> lot)
      SubmitEnd -> pure ([chg], [])
      AddEvent e -> do
        ident <- use nextId
        nextId += 1
        events . at ident ?= e
        pure ([chg], [])
      RemoveEvent ident -> do
        events . at ident .= Nothing
        pure ([chg], [])
      ReplaceEvent ident e -> do
        events . at ident ?= e
        pure ([chg], [])

impliedChanges :: Timed Action -> State JournalState [Change]
impliedChanges x = do
  hist <- gets (sortOn fst . toListOf (events . ifolded . withIndex))
  let (mx, changes) =
        runWriter $ (\f -> foldM f (Just x) (tails hist)) $ flip $
          \hs -> \case
            Nothing -> pure Nothing
            Just x' -> handle hs x'
  case mx of
    Just x' -> error $ "impliedChanges: unexpected remainder: " ++ show x'
    Nothing -> pure changes

buyOrSell :: Traversal' (Timed Action) Lot
buyOrSell = item . failing _Buy _Sell

adjust :: Traversal' (Timed Action) Lot
adjust = item . _Adjust

opened :: Traversal' (Timed Event) Lot
opened = item . _Opened . _2

adjustment :: Traversal' (Timed Event) Lot
adjustment = item . _Adjustment

handle ::
  [(Int, Timed Event)] ->
  Timed Action ->
  Writer [Change] (Maybe (Timed Action))
handle
  ((n, open@(view item -> Opened buyToOpen _)) : _)
  close@(preview buyOrSell -> Just _)
    | buyToOpen == has (item . _Sell) close =
      closePosition n open close
handle
  ((n, open@(view item -> Opened _ o)) : _)
  wash@(preview adjust -> Just _)
    | all (/= PartWashed) (o ^. details),
      (open ^. time) `distance` (wash ^. time) <= 30 =
      washExistingPosition n open wash
handle
  ((n, adj@(view item -> Adjustment _)) : _)
  open@(preview buyOrSell -> Just _)
    | (adj ^. time) `distance` (open ^. time) <= 30 =
      -- We are buying or selling to open, but must wash first
      washNewPosition n adj open
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] act@(preview buyOrSell -> Just open) = do
  tell
    [ AddEvent (Opened (has (item . _Buy) act) open <$ act),
      Result (act & buyOrSell . details <>~ [Position Open])
    ]
  pure Nothing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] act@(preview adjust -> Just x) = do
  tell [AddEvent (Adjustment x <$ act)]
  pure Nothing
-- If none of the above apply, nothing is done for this action, pass through
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
-- to close an existing one for a profit or loss. If there is a loss within 30
-- days of the opening, it implies a wash sale adjustment of any preceeding or
-- subsequent openings 30 days before or after.
closePosition ::
  Int ->
  Timed Event ->
  Timed Action ->
  Writer [Change] (Maybe (Timed Action))
closePosition n open close = do
  let (s, d) = (open ^?! opened) `alignLots` (close ^?! buyOrSell)
  forM_ ((,) <$> s ^? _SplitUsed <*> d ^? _SplitUsed) $ \(su, du) -> do
    let pl
          | has (item . _Sell) close =
            du ^. price - su ^. price
          | otherwise = su ^. price - du ^. price
        res =
          du & details
            <>~ [ Position Close,
                  if pl < 0
                    then Loss (- pl)
                    else Gain pl
                ]
    tell [Result (close & buyOrSell .~ res)]
    -- After closing at a loss, and if the loss occurs within 30 days
    -- of its corresponding open, and there is another open within 30
    -- days of the loss, close it and re-open so it's repriced by the
    -- wash loss.
    let mayWash = (close ^. time) `distance` (open ^. time) <= 30 && pl < 0
    tell
      [ case s ^? _SplitKept of
          Just k ->
            ReplaceEvent
              n
              ( open & opened
                  .~ if mayWash
                    then k & details <>~ [PartWashed]
                    else k
              )
          Nothing -> RemoveEvent n
      ]
    when mayWash $
      tell [Submit ((du & price .~ negate pl) <$ close)]
  pure $ (set buyOrSell ?? close) <$> (d ^? _SplitKept)

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washExistingPosition ::
  Int ->
  Timed Event ->
  Timed Action ->
  Writer [Change] (Maybe (Timed Action))
washExistingPosition n open wash =
  assert (wash ^?! adjust . amount /= 0) $ do
    let (s, d) = (open ^?! opened) `alignLots` (wash ^?! adjust)
    -- Wash failing closes by adding the amount to the cost basis of the
    -- opening transaction.
    tell $
      [ case s ^? _SplitKept of
          Just e -> ReplaceEvent n (open & opened .~ e)
          Nothing -> RemoveEvent n
      ]
        ++ ( AddEvent
               . (set opened ?? open)
               . (details <>~ [Washed Retroactively (d ^?! _SplitUsed . price)])
               <$> s ^.. _SplitUsed
           )
    pure $ (set adjust ?? wash) <$> (d ^? _SplitKept)

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washNewPosition ::
  Int ->
  Timed Event ->
  Timed Action ->
  Writer [Change] (Maybe (Timed Action))
washNewPosition n adj open = do
  let (s, d) = (adj ^?! adjustment) `alignLots` (open ^?! buyOrSell)
  -- We wash failing closes by adding the amount to the cost basis of
  -- the opening transaction. Thus, we generate three instances of
  -- WashLossApplied, but only one OpenPosition.
  tell $
    [ case s ^? _SplitKept of
        Just e -> ReplaceEvent n (adj & adjustment .~ e)
        Nothing -> RemoveEvent n
    ]
      ++ ( AddEvent
             . Timed (open ^. time)
             . Opened (has (item . _Buy) open)
             . (details <>~ [Washed OnOpen (adj ^?! adjustment . price)])
             <$> d ^.. _SplitUsed
         )
      ++ ( Result . (set buyOrSell ?? open)
             . ( details
                   <>~ [ Position Open,
                         Washed OnOpen (adj ^?! adjustment . price)
                       ]
               )
             <$> d ^.. _SplitUsed
         )
  pure $ (set buyOrSell ?? open) <$> d ^? _SplitKept
