{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.GainsKeeper
  ( processAction,
    InstrumentState,
    newInstrumentState,
    Event (..),
    Change (..),
    gainsKeeper,
    GainsKeeperError,
    ActionLike (..),
  )
where

import Control.Applicative
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.IntMap (IntMap)
import Data.List (sortOn, tails)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types hiding (Action, _Buy, _Sell, _Wash)
import Journal.Utils
import Pipes
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

data Event
  = Opened Bool Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Event

opened :: Traversal' Event Lot
opened = _Opened . _2

data Change a
  = SawAction a
  | Submit Lot -- can only be an adjustment
  | SubmitEnd
  | Result a
  | AddEvent Event
  | RemoveEvent Int
  | ReplaceEvent Int Event
  | SaveWash Text Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''Change

data InstrumentState = InstrumentState
  { _events :: IntMap (Annotated Event),
    _washSales :: Map Text [Annotated Lot]
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''InstrumentState

newInstrumentState :: InstrumentState
newInstrumentState = InstrumentState mempty mempty

data GainsKeeperError a
  = ChangeNotFromImpliedChanges (Annotated (Change a))
  | UnexpectedRemainder (Annotated a)
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

class ActionLike a where
  _Buy :: Prism' a Lot
  _Sell :: Prism' a Lot
  _Wash :: Prism' a Lot

  buyOrSell :: Traversal' a Lot
  buyOrSell = failing _Buy _Sell

data GainsKeeperState = GainsKeeperState
  { _nextId :: Int,
    _instruments :: Map Text InstrumentState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

symbolName :: ActionLike a => Traversal' a Text
symbolName f s = s & failing _Buy (failing _Sell _Wash) . symbol %%~ f

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState
newGainsKeeperState =
  GainsKeeperState
    { _nextId = 1,
      _instruments = mempty
    }

gainsKeeper ::
  (MonadError (GainsKeeperError a) m, Show a, ActionLike a) =>
  Pipe (Annotated a) (Annotated (Change a)) m r
gainsKeeper = flip evalStateT newGainsKeeperState $
  forever $ do
    entry <- lift await
    results <-
      zoom
        ( zipped
            nextId
            ( instruments . at (entry ^. item . symbolName)
                . non newInstrumentState
            )
        )
        $ do
          processAction entry
    forM_ results $ lift . yield

processAction ::
  (MonadError (GainsKeeperError a) m, Show a, ActionLike a) =>
  Annotated a ->
  StateT
    (Int, InstrumentState)
    m
    [Annotated (Change a)]
processAction =
  fmap concat . mapM applyChange <=< readonly . impliedChanges
  where
    applyChange chg = case chg ^. item of
      Submit lot ->
        (\xs -> chg : xs ++ [SubmitEnd <$ chg])
          <$> processAction ((_Wash #) <$> (lot <$ chg))
      AddEvent e -> do
        ident <- use _1
        _1 += 1
        _2 . events . at ident ?= (e <$ chg)
        pure [chg]
      RemoveEvent ident -> do
        _2 . events . at ident .= Nothing
        pure [chg]
      ReplaceEvent ident e -> do
        _2 . events . at ident ?= (e <$ chg)
        pure [chg]
      SaveWash name lot -> do
        _2 . washSales . at name
          %= \case
            Nothing -> Just [lot <$ chg]
            Just xs -> Just ((lot <$ chg) : xs)
        pure [chg]
      SawAction _ -> pure [chg]
      Result _ -> pure [chg]
      SubmitEnd -> pure [chg]

impliedChanges ::
  (MonadError (GainsKeeperError a) m, Show a, ActionLike a) =>
  Annotated a ->
  ReaderT (Int, InstrumentState) m [Annotated (Change a)]
impliedChanges x = do
  hist <- asks (sortOn fst . toListOf (_2 . events . ifolded . withIndex))
  (mx, changes) <-
    runWriterT $ foldAM x (tails hist) $ maybe (pure Nothing) . handle
  forM_ mx $ throwError . UnexpectedRemainder
  pure ((SawAction <$> x) : changes)

handle ::
  (Monad m, Show a, ActionLike a) =>
  [(Int, Annotated Event)] ->
  Annotated a ->
  WriterT
    [Annotated (Change a)]
    (ReaderT (Int, InstrumentState) m)
    (Maybe (Annotated a))
handle
  ((n, open@(view item -> Opened buyToOpen _)) : _)
  close@(preview (item . buyOrSell) -> Just _)
    | buyToOpen == has (item . _Sell) close =
      closePosition n open close
handle
  ((n, open@(view item -> Opened _ _)) : _)
  washing@(preview (item . _Wash) -> Just _)
    | all
        (\ann -> hasn't _Exempt ann && hasn't _Washed ann)
        (open ^. details),
      (open ^?! time) `distance` (washing ^?! time) <= 30 =
      washExistingPosition n open washing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] act@(preview (item . _Wash) -> Just x) = do
  case act ^? details . traverse . _WashTo of
    Just (name, mres) ->
      Nothing
        <$ tell
          [ SaveWash
              name
              <$> ( ( case mres of
                        Nothing -> x
                        Just (_amount, _price) ->
                          let _symbol = x ^. symbol
                           in Lot {..}
                    )
                      <$ act
                  ),
            Result <$> (_Wash # x <$ act)
          ]
    _ -> pure Nothing
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] action = do
  mact <- foldAM
    action
    (action ^.. details . traverse . _WashApply)
    $ \(name, _amount) -> \case
      Just (act@(preview (item . buyOrSell) -> Just open)) -> do
        sales <-
          lift $
            asks
              ( toListOf
                  ( _2 . washSales
                      . ix name
                      . folded
                      . filtered
                        ( \adj ->
                            (adj ^?! time)
                              `distance` (act ^?! time) <= 30
                        )
                      . to (\x -> x ^. item . amount * x ^. item . price)
                  )
              )
        let _price = sum sales / _amount
            _symbol = open ^. symbol
        washNewPosition Lot {..} act
      _ -> pure Nothing
  case mact of
    Just (act@(preview (item . buyOrSell) -> Just open)) ->
      Nothing
        <$ tell
          [ AddEvent <$> (Opened (has (item . _Buy) act) open <$ act),
            Result <$> (act & details <>~ [Position Open])
          ]
    _ ->
      -- If none of the above apply, nothing is done for this action, pass
      -- it through
      pure mact
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
--   to close an existing one for a profit or loss. If there is a loss within
--   30 days of the opening, it implies a wash sale adjustment of any
--   preceeding or subsequent openings 30 days before or after.
closePosition ::
  (Monad m, Show a, ActionLike a) =>
  Int ->
  Annotated Event ->
  Annotated a ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
closePosition n open close = do
  let (s, d) = (open ^?! item . opened) `alignLots` (close ^?! item . buyOrSell)
  forM_ ((,) <$> s ^? _SplitUsed <*> d ^? _SplitUsed) $ \(su, du) -> do
    let pricing o x = x ^. price + sum (o ^.. details . traverse . _Washed)
        lotFees = sum (close ^.. fees) + sum (open ^.. fees)
        pl
          | has (item . _Sell) close =
            pricing close du - pricing open su - lotFees
          | otherwise = pricing open su - pricing close du - lotFees
    tell
      [ Result
          <$> ( close & item . buyOrSell .~ du
                  & details
                  <>~ [ Position Close,
                        if pl < 0
                          then Loss (- pl)
                          else Gain pl
                      ]
              )
      ]
    -- After closing at a loss, and if the loss occurs within 30 days
    -- of its corresponding open, and there is another open within 30
    -- days of the loss, close it and re-open so it's repriced by the
    -- wash loss.
    let mayWash = (close ^?! time) `distance` (open ^?! time) <= 30 && pl < 0
    tell
      [ case s ^? _SplitKept of
          Just k ->
            ReplaceEvent
              n
              <$> ( open & item . opened .~ k
                      & details <>~ [Exempt | mayWash]
                  )
          Nothing -> RemoveEvent n <$ close
      ]
    when mayWash $
      tell
        [ Submit <$> ((du & price .~ negate pl) <$ close)
        ]
  pure $ (\x -> close & item . buyOrSell .~ x) <$> d ^? _SplitKept

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washExistingPosition ::
  (Monad m, ActionLike a) =>
  Int ->
  Annotated Event ->
  Annotated a ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
washExistingPosition n open washing =
  assert (washing ^?! item . _Wash . amount /= 0) $ do
    let (s, d) = (open ^?! item . opened) `alignLots` (washing ^?! item . _Wash)
    -- Wash failing closes by adding the amount to the cost basis of the
    -- opening transaction.
    tell $
      [ case s ^? _SplitKept of
          Just e ->
            ReplaceEvent n
              <$> (open & item . opened .~ e & details <>~ [Exempt])
          Nothing -> RemoveEvent n <$ open
      ]
        ++ ( fmap AddEvent
               . ( \x ->
                     open & item . opened .~ x
                       & details <>~ [Washed (d ^?! _SplitUsed . price)]
                 )
               <$> s ^.. _SplitUsed
           )
        ++ ( ( \x ->
                 washing & item .~ Result (_Wash # x)
                   & details
                     .~ [ Time (washing ^?! time),
                          Washed (s ^?! _SplitUsed . price)
                        ]
             )
               <$> d ^.. _SplitUsed
           )
    pure $ (\x -> washing & item . _Wash .~ x) <$> d ^? _SplitKept

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washNewPosition ::
  (Monad m, ActionLike a) =>
  Lot ->
  Annotated a ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
washNewPosition washing open =
  assert (washing ^. amount <= open ^?! item . buyOrSell . amount) $ do
    let (_s, d) = washing `alignLots` (open ^?! item . buyOrSell)
    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    tell $
      ( ( \x ->
            open & item .~ AddEvent (Opened (has (item . _Buy) open) x)
              & details <>~ [Washed (washing ^. price)]
        )
          <$> d ^.. _SplitUsed
      )
        ++ ( fmap Result
               . ( \x ->
                     open & item . buyOrSell .~ x
                       & details
                         <>~ [ Position Open,
                               Washed (washing ^. price)
                             ]
                 )
               <$> d ^.. _SplitUsed
           )
    pure $ (\x -> open & item . buyOrSell .~ x) <$> d ^? _SplitKept
