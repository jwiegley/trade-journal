{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Model
  ( processJournal,
    processJournalWithChanges,
    processActionsWithChanges,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List (sortOn, tails)
import Journal.Amount
import Journal.Split
import Journal.Types
import Journal.Utils
import Prelude hiding (Double, Float)

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processJournal :: MonadError JournalError m => Journal -> m Journal
processJournal = fmap snd . processJournalWithChanges

processJournalWithChanges ::
  MonadError JournalError m =>
  Journal ->
  m ([Change], Journal)
processJournalWithChanges =
  fmap (second Journal) . processActionsWithChanges . view actions

processActionsWithChanges ::
  MonadError JournalError m =>
  [Timed Action] ->
  m ([Change], [Timed Action])
processActionsWithChanges xs =
  fmap unzipBoth . (`evalStateT` newJournalState) $ do
    forM xs $ \x -> do
      let lot = x ^. item . _Lot
          macct = lot ^? details . traverse . _Account
          sym = lot ^. symbol
      zoom (accounts . at macct . non newAccountState) $ do
        zoom
          ( zipped3
              nextId
              balance
              (instruments . at sym . non newInstrumentState)
          )
          $ first (SawAction x :) <$> processAction x

checkNetAmount ::
  MonadError JournalError m =>
  Timed Action ->
  m (Timed Action)
checkNetAmount x
  | Just itemNet <- x ^? item . _Lot . details . traverse . _Net = do
    let calcNet = netAmount (x ^. item)
    if itemNet == calcNet
      then pure x
      else throwError $ NetAmountDoesNotMatch x calcNet itemNet
  | otherwise =
    pure $ x & item . _Lot . details <>~ [Net (netAmount (x ^. item))]

checkBalance ::
  MonadError JournalError m =>
  Timed Action ->
  StateT (Amount 2) m (Timed Action)
checkBalance x
  | Just itemBal <- x ^? item . _Lot . details . traverse . _Balance = do
    bal <- get
    let newBal = netAmount (x ^. item) + bal
    if itemBal == newBal
      then pure x
      else throwError $ BalanceDoesNotMatch x newBal itemBal
  | otherwise = do
    bal <- get
    let newBal = netAmount (x ^. item) + bal
    put newBal
    pure $ x & item . _Lot . details <>~ [Balance newBal]

processAction ::
  MonadError JournalError m =>
  Timed Action ->
  StateT (Int, Amount 2, InstrumentState) m ([Change], [Timed Action])
processAction =
  fmap unzipBoth . mapM applyChange <=< readonly . impliedChanges
  where
    applyChange chg = case chg of
      SawAction _ -> throwError $ ChangeNotFromImpliedChanges chg
      Result e -> do
        e' <- zoom _2 $ checkBalance e
        e'' <- lift $ checkNetAmount e'
        pure ([chg], [e''])
      Submit lot ->
        first (\xs -> chg : xs ++ [SubmitEnd])
          <$> processAction (Wash <$> lot)
      SubmitEnd -> pure ([chg], [])
      AddEvent e -> do
        ident <- use _1
        _1 += 1
        _3 . events . at ident ?= e
        pure ([chg], [])
      RemoveEvent ident -> do
        _3 . events . at ident .= Nothing
        pure ([chg], [])
      ReplaceEvent ident e -> do
        _3 . events . at ident ?= e
        pure ([chg], [])
      SaveWash name lot -> do
        _3 . washSales . at name
          %= \case Nothing -> Just [lot]; Just xs -> Just (lot : xs)
        pure ([chg], [])

impliedChanges ::
  MonadError JournalError m =>
  Timed Action ->
  ReaderT (Int, Amount 2, InstrumentState) m [Change]
impliedChanges x = do
  hist <- asks (sortOn fst . toListOf (_3 . events . ifolded . withIndex))
  (mx, changes) <-
    runWriterT $ foldAM x (tails hist) $ maybe (pure Nothing) . handle
  forM_ mx $ throwError . UnexpectedRemainder
  pure changes

buyOrSell :: Traversal' (Timed Action) Lot
buyOrSell = item . failing _Buy _Sell

wash :: Traversal' (Timed Action) Lot
wash = item . _Wash

opened :: Traversal' (Timed Event) Lot
opened = item . _Opened . _2

handle ::
  MonadError JournalError m =>
  [(Int, Timed Event)] ->
  Timed Action ->
  WriterT
    [Change]
    (ReaderT (Int, Amount 2, InstrumentState) m)
    (Maybe (Timed Action))
handle
  ((n, open@(view item -> Opened buyToOpen _)) : _)
  close@(preview buyOrSell -> Just _)
    | buyToOpen == has (item . _Sell) close =
      closePosition n open close
handle
  ((n, open@(view item -> Opened _ o)) : _)
  washing@(preview wash -> Just _)
    | all
        (\ann -> hasn't _Exempt ann && hasn't _Washed ann)
        (o ^. details),
      (open ^. time) `distance` (washing ^. time) <= 30 =
      washExistingPosition n open washing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] act@(preview wash -> Just x) = do
  case x ^? details . traverse . _WashTo of
    Just (name, mres) ->
      Nothing
        <$ tell
          [ SaveWash
              name
              ( ( case mres of
                    Nothing -> x
                    Just (_amount, _price) ->
                      let _symbol = x ^. symbol
                          _details = []
                          _computed = []
                       in Lot {..}
                )
                  <$ act
              ),
            Result (Wash x <$ act)
          ]
    _ -> pure Nothing
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] action = do
  mact <- foldAM
    action
    (action ^.. buyOrSell . details . traverse . _WashApply)
    $ \(name, _amount) -> \case
      Just (act@(preview buyOrSell -> Just open)) -> do
        sales <-
          lift $
            asks
              ( toListOf
                  ( _3 . washSales
                      . ix name
                      . folded
                      . filtered
                        ( \adj ->
                            (adj ^. time)
                              `distance` (act ^. time) <= 30
                        )
                      . to (\x -> x ^. item . amount * x ^. item . price)
                  )
              )
        let _price = sum sales / _amount
            _symbol = open ^. symbol
            _details = []
            _computed = []
        washNewPosition Lot {..} act
      _ -> pure Nothing
  case mact of
    Just (act@(preview buyOrSell -> Just open)) ->
      Nothing
        <$ tell
          [ AddEvent (Opened (has (item . _Buy) act) open <$ act),
            Result (act & buyOrSell . details <>~ [Position Open])
          ]
    _ ->
      -- If none of the above apply, nothing is done for this action, pass
      -- it through
      pure mact
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
-- to close an existing one for a profit or loss. If there is a loss within 30
-- days of the opening, it implies a wash sale adjustment of any preceeding or
-- subsequent openings 30 days before or after.
closePosition ::
  MonadError JournalError m =>
  Int ->
  Timed Event ->
  Timed Action ->
  WriterT [Change] m (Maybe (Timed Action))
closePosition n open close = do
  let (s, d) = (open ^?! opened) `alignLots` (close ^?! buyOrSell)
  forM_ ((,) <$> s ^? _SplitUsed <*> d ^? _SplitUsed) $ \(su, du) -> do
    let pricing x =
          x ^. price
            + sum
              ( x
                  ^.. details
                    . traverse
                    . _Washed
              )
        lotFees = sum (du ^.. fees) + sum (su ^.. fees)
        pl
          | has (item . _Sell) close =
            pricing du - pricing su - lotFees
          | otherwise = pricing su - pricing du - lotFees
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
                    then k & details <>~ [Exempt]
                    else k
              )
          Nothing -> RemoveEvent n
      ]
    when mayWash $
      tell
        [ Submit ((du & price .~ negate pl) <$ close)
        ]
  pure $ (set buyOrSell ?? close) <$> (d ^? _SplitKept)

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washExistingPosition ::
  MonadError JournalError m =>
  Int ->
  Timed Event ->
  Timed Action ->
  WriterT [Change] m (Maybe (Timed Action))
washExistingPosition n open washing =
  assert (washing ^?! wash . amount /= 0) $ do
    let (s, d) = (open ^?! opened) `alignLots` (washing ^?! wash)
    -- Wash failing closes by adding the amount to the cost basis of the
    -- opening transaction.
    tell $
      [ case s ^? _SplitKept of
          Just e ->
            ReplaceEvent n (set opened (e & details <>~ [Exempt]) open)
          Nothing -> RemoveEvent n
      ]
        ++ ( AddEvent
               . (set opened ?? open)
               . (details <>~ [Washed (d ^?! _SplitUsed . price)])
               <$> s ^.. _SplitUsed
           )
        ++ ( Result . (set wash ?? washing)
               . (details .~ [Washed (s ^?! _SplitUsed . price)])
               <$> d ^.. _SplitUsed
           )
    pure $ (set wash ?? washing) <$> (d ^? _SplitKept)

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washNewPosition ::
  MonadError JournalError m =>
  Lot ->
  Timed Action ->
  WriterT [Change] m (Maybe (Timed Action))
washNewPosition washing open =
  assert (washing ^. amount <= open ^?! buyOrSell . amount) $ do
    let (_s, d) = washing `alignLots` (open ^?! buyOrSell)
    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    tell $
      ( AddEvent
          . Timed (open ^. time)
          . Opened (has (item . _Buy) open)
          . (details <>~ [Washed (washing ^. price)])
          <$> d ^.. _SplitUsed
      )
        ++ ( Result . (set buyOrSell ?? open)
               . ( details
                     <>~ [ Position Open,
                           Washed (washing ^. price)
                         ]
                 )
               <$> d ^.. _SplitUsed
           )
    pure $ (set buyOrSell ?? open) <$> d ^? _SplitKept
