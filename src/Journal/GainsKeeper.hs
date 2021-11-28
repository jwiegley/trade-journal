{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.GainsKeeper
  ( GainsEvent (..),
    Change (..),
    gainsKeeper,
    pickResults,
    GainsKeeperError,
    ActionLike (..),
  )
where

import Amount
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.IntMap (IntMap)
import Data.List (sortOn, tails)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time
import GHC.Generics hiding (to)
import Journal.Split
import Journal.Types hiding
  ( Action,
    _Buy,
    _Close,
    _Open,
    _Sell,
    _Wash,
  )
import Journal.Utils
import Pipes
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

data GainsEvent
  = Opened Bool [Int] Lot
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makePrisms ''GainsEvent

openIds :: Traversal' GainsEvent [Int]
openIds = _Opened . _2

opened :: Traversal' GainsEvent Lot
opened = _Opened . _3

data Change a
  = SawAction a
  | SubmitWash UTCTime Lot
  | SubmitEnd
  | Result a
  | AddEvent GainsEvent
  | RemoveEvent Int
  | ReplaceEvent Int GainsEvent
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
  { _events :: IntMap (Annotated GainsEvent),
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
  _Open :: Prism' a (Disposition, Lot)
  _Close :: Prism' a (Disposition, Lot, Amount 6)
  _Wash :: Prism' a (Period, UTCTime, Lot)

  buyOrSell :: Traversal' a Lot
  buyOrSell = failing _Buy _Sell

data GainsKeeperState = GainsKeeperState
  { gkNextId :: Int,
    gkNextEventId :: Int,
    gkInstruments :: Map Text InstrumentState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

data LocalState = LocalState
  { _nextId :: Int,
    _nextEventId :: Int,
    _instrumentState :: InstrumentState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''LocalState

localState :: Text -> Traversal' GainsKeeperState LocalState
localState instrument f GainsKeeperState {..} =
  let s = gkInstruments ^. at instrument . non newInstrumentState
   in f (LocalState gkNextId gkNextEventId s) <&> \lst ->
        GainsKeeperState
          { gkNextId = _nextId lst,
            gkNextEventId = _nextEventId lst,
            gkInstruments =
              gkInstruments & at instrument ?~ _instrumentState lst
          }

symbolName :: ActionLike a => Traversal' a Text
symbolName f s =
  s & failing _Buy (failing _Sell (_Wash . _3)) . symbol %%~ f

newGainsKeeperState :: GainsKeeperState
newGainsKeeperState =
  GainsKeeperState
    { gkNextId = 0,
      gkNextEventId = 1,
      gkInstruments = mempty
    }

gainsKeeper ::
  (MonadError (GainsKeeperError a) m, Show a, ActionLike a) =>
  Pipe (Annotated a) (Annotated (Change a)) m r
gainsKeeper = flip evalStateT newGainsKeeperState $
  forever $ do
    entry <- lift await
    results <-
      zoom (localState (entry ^. item . symbolName)) $
        processAction entry
    forM_ results $ lift . yield

pickResults :: Functor m => Pipe (Annotated (Change a)) (Annotated a) m r
pickResults = forever $ do
  change <- await
  case change ^. item of
    Result x -> yield (x <$ change)
    _ -> pure ()

processAction ::
  (MonadError (GainsKeeperError a) m, Show a, ActionLike a) =>
  Annotated a ->
  StateT
    LocalState
    m
    [Annotated (Change a)]
processAction =
  fmap concat . mapM applyChange <=< impliedChanges
  where
    applyChange chg = case chg ^. item of
      SubmitWash moment a ->
        (\xs -> chg : xs ++ [SubmitEnd <$ chg])
          <$> processAction ((_Wash # (Future, moment, a)) <$ chg)
      AddEvent e -> do
        ident <- use nextEventId
        nextEventId += 1
        instrumentState . events . at ident ?= (e <$ chg)
        pure [chg]
      RemoveEvent ident -> do
        instrumentState . events . at ident .= Nothing
        pure [chg]
      ReplaceEvent ident e -> do
        instrumentState . events . at ident ?= (e <$ chg)
        pure [chg]
      SaveWash name lot -> do
        instrumentState . washSales . at name
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
  StateT LocalState m [Annotated (Change a)]
impliedChanges x = do
  hist <-
    gets
      ( sortOn fst
          . toListOf (instrumentState . events . ifolded . withIndex)
      )
  (mx, changes) <-
    runWriterT $ foldAM x (tails hist) $ maybe (pure Nothing) . handle
  forM_ mx $ throwError . UnexpectedRemainder
  pure ((SawAction <$> x) : changes)

handle ::
  (Monad m, Show a, ActionLike a) =>
  [(Int, Annotated GainsEvent)] ->
  Annotated a ->
  WriterT
    [Annotated (Change a)]
    (StateT LocalState m)
    (Maybe (Annotated a))
handle
  ((n, open@(view item -> Opened buyToOpen _ _)) : _)
  close@(preview (item . buyOrSell) -> Just _)
    | buyToOpen == has (item . _Sell) close =
      closePosition n open close
handle
  ((n, open@(has (item . _Opened) -> True)) : _)
  washing@(preview (item . _Wash) -> Just (Future, _, _))
    | all
        (\ann -> hasn't _Exempt ann && hasn't _Washed ann)
        (open ^.. details . traverse),
      (open ^. time) `distance` (washing ^. time) <= 30 =
      washPast n open washing
-- If a wash sale couldn't be applied to the current history, record it to
-- apply to a future opening.
handle [] act@(preview (item . _Wash) -> Just (Future, moment, x)) =
  washFuture act moment x
-- Otherwise, if there is no history to examine then this buy or sale must
-- open a new position.
handle [] action = openPosition action
handle _ x = pure $ Just x

-- | The most common trading activities are either to open a new position, or
--   to close an existing one for a profit or loss. If there is a loss within
--   30 days of the opening, it implies a wash sale adjustment of any
--   preceeding or subsequent openings 30 days before or after.
openPosition ::
  (Monad m, Show a, ActionLike a) =>
  Annotated a ->
  WriterT
    [Annotated (Change a)]
    (StateT LocalState m)
    (Maybe (Annotated a))
openPosition action = do
  lift $ nextId += 1
  ident <- lift $ use nextId

  -- An application of a wash sale amount is not something that this code ever
  -- generates, but it may be specified manually by the user when they know
  -- that a wash sale is applicable (based on information from their broker).
  mact <- foldAM
    action
    (action ^.. details . traverse . _WashApply)
    $ \(name, _amount) -> \case
      Just (act@(preview (item . buyOrSell) -> Just open)) -> do
        sales <-
          lift $
            gets
              ( toListOf
                  ( instrumentState
                      . washSales
                      . ix name
                      . folded
                      . filtered
                        ( \adj ->
                            (adj ^. time) `distance` (act ^. time) <= 30
                        )
                      . to (\x -> x ^. item . amount * x ^. item . price)
                  )
              )
        let _price = sum sales / _amount
            _symbol = open ^. symbol
        washPresent Lot {..} [ident] act
      _ -> pure Nothing

  case mact of
    Just (act@(preview (item . buyOrSell) -> Just open)) ->
      Nothing
        <$ tell
          [ AddEvent
              <$> (Opened (has (item . _Buy) act) [ident] open <$ act),
            Result <$> act,
            Result
              <$> ( ( ( _Open
                          # ( ( if has (item . _Buy) act
                                  then Long
                                  else Short
                              ),
                              open
                            )
                      )
                        <$ act
                    )
                      & details <>~ [Idents [ident]]
                  )
          ]
    _ ->
      -- If none of the above apply, nothing is done for this action, pass it
      -- through
      pure mact

-- | The most common trading activities are either to open a new position, or
--   to close an existing one for a profit or loss. If there is a loss within
--   30 days of the opening, it implies a wash sale adjustment of any
--   preceeding or subsequent openings 30 days before or after.
closePosition ::
  (Monad m, Show a, ActionLike a) =>
  Int ->
  Annotated GainsEvent ->
  Annotated a ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
closePosition n open close = do
  let (s, d) = (open ^?! item . opened) `align` (close ^?! item . buyOrSell)
  forM_ ((,) <$> s ^? _SplitUsed <*> d ^? _SplitUsed) $ \(su, du) -> do
    let lotFees = sum (close ^.. fees) + sum (open ^.. fees)
        pricing o x =
          x ^. price
            + sum (o ^.. details . traverse . _Washed)
        pl
          | has (item . _Sell) close =
            pricing close du - pricing open su - lotFees
          | otherwise = pricing open su - pricing close du - lotFees
    tell
      [ Result <$> (close & item . buyOrSell .~ du),
        Result
          <$> ( ( _Close
                    # ( if has (item . _Sell) close
                          then Long
                          else Short,
                        close ^?! item . buyOrSell,
                        pl
                      )
                    <$ close
                )
                  & details
                  <>~ [Idents (open ^?! item . openIds)]
              )
      ]
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
              <$> ( open & item . opened .~ k
                      & details <>~ [Exempt | mayWash]
                  )
          Nothing -> RemoveEvent n <$ close
      ]
    when mayWash $
      tell
        [ SubmitWash (close ^. time)
            <$> ((du & price .~ negate pl) <$ close)
        ]
  pure $ (\x -> close & item . buyOrSell .~ x) <$> d ^? _SplitKept

-- | If the action is a wash sale adjustment, determine if can be applied to
-- any existing open positions. If not, it is remembered, to be applied at the
-- next opening within 30 days of sale.
washPast ::
  (Monad m, ActionLike a) =>
  Int ->
  Annotated GainsEvent ->
  Annotated a ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
washPast n open washing =
  assert (washing ^?! item . _Wash . _3 . amount /= 0) $ do
    let (s, d) =
          (open ^?! item . opened)
            `align` (washing ^?! item . _Wash . _3)
    -- Wash failing closes by adding the amount to the cost basis of the
    -- opening transaction.
    tell
      [ case s ^? _SplitKept of
          Just e ->
            ReplaceEvent n
              <$> (open & item . opened .~ e & details <>~ [Exempt])
          Nothing -> RemoveEvent n <$ open
      ]
    forM_ (s ^.. _SplitUsed) $ \x ->
      tell
        [ AddEvent
            <$> ( open & item . opened .~ x
                    & details <>~ [Washed (d ^?! _SplitUsed . price)]
                )
        ]
    forM_ (d ^.. _SplitUsed) $ \x ->
      tell
        [ washing & item .~ Result (_Wash # (Past, washing ^. time, x))
            & time .~ (washing ^. time)
            & details
              <>~ [Idents (open ^?! item . openIds)]
        ]
    pure $ (\x -> washing & item . _Wash . _3 .~ x) <$> d ^? _SplitKept

-- | If we are opening a position and there is a pending wash sale within the
-- last 30 days, apply the adjustment to the applicable part of this opening.
washPresent ::
  (Monad m, ActionLike a) =>
  Lot ->
  [Int] ->
  Annotated a ->
  WriterT
    [Annotated (Change a)]
    (StateT LocalState m)
    (Maybe (Annotated a))
washPresent washing ids open =
  assert (washing ^. amount <= open ^?! item . buyOrSell . amount) $ do
    let (_s, d) = washing `align` (open ^?! item . buyOrSell)
    -- We wash failing closes by adding the amount to the cost basis of
    -- the opening transaction. Thus, we generate three instances of
    -- WashLossApplied, but only one OpenPosition.
    forM_ (d ^.. _SplitUsed) $ \x ->
      tell
        [ open & item .~ AddEvent (Opened (has (item . _Buy) open) ids x)
            & details <>~ [Washed (washing ^. price)],
          Result <$> (open & item . buyOrSell .~ x),
          Result
            <$> ( ( ( _Open
                        # ( if has (item . _Buy) open
                              then Long
                              else Short,
                            open ^?! item . buyOrSell
                          )
                    )
                      <$ open
                  )
                    & details
                    <>~ [ Idents ids,
                          Washed (washing ^. price)
                        ]
                ),
          Result
            <$> ((_Wash # (Present, open ^. time, washing)) <$ open)
        ]
    pure $ (\x -> open & item . buyOrSell .~ x) <$> d ^? _SplitKept

-- | If a wash sale adjustment could not be applied, record it in case a
-- related position is opened within the next 30 days.
washFuture ::
  (Monad m, ActionLike a) =>
  Annotated a ->
  UTCTime ->
  Lot ->
  WriterT [Annotated (Change a)] m (Maybe (Annotated a))
washFuture act moment lot = do
  case act ^? details . traverse . _WashTo of
    Just (name, mres) ->
      Nothing
        <$ tell
          [ SaveWash
              name
              <$> ( ( case mres of
                        Nothing -> lot
                        Just (_amount, _price) ->
                          let _symbol = lot ^. symbol
                           in Lot {..}
                    )
                      <$ act
                  ),
            Result <$> (_Wash # (Future, moment, lot) <$ act)
          ]
    _ -> pure Nothing
