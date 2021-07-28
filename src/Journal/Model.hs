{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC "-Wno-orphans" #-}

module Journal.Model where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics hiding (to)
import Journal.Amount
import qualified Journal.GainsKeeper as Gains
import Journal.Types
import Journal.Utils
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data AccountState = AccountState
  { _nextId :: Int,
    _balance :: Amount 2,
    _instruments :: Map Text Gains.InstrumentState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''AccountState

newAccountState :: AccountState
newAccountState = AccountState 1 0 mempty

data JournalState = JournalState
  { _accounts :: Map (Maybe Text) AccountState
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic
    )

makeLenses ''JournalState

newJournalState :: JournalState
newJournalState = JournalState mempty

data Journal = Journal
  { _actions :: [Annotated Action]
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makeLenses ''Journal

newJournal :: Journal
newJournal = Journal []

instance Gains.ActionLike Action where
  _Buy = _Buy
  _Sell = _Sell
  _Wash = _Wash

-- data JournalError
--   = FromGainsKeeper GainsKeeperError
--   | UnexpectedRemainder (Annotated Action)
--   | NetAmountDoesNotMatch (Annotated Action) (Amount 2) (Amount 2)
--   | BalanceDoesNotMatch (Annotated Action) (Amount 2) (Amount 2)
--   deriving
--     ( Show,
--       Eq,
--       Ord,
--       Generic,
--       PrettyVal
--     )

-- "Change not produced by impliedChanges"
-- "impliedChanges: unexpected remainder: "
-- "Unapplied wash sale requires use of \"wash to\""

-- | Ideally, this module turns a stream of lots -- expressing intentions to
--   buy and sell at given prices -- into a record of transactions with the
--   broker where all gains and losses have been calculated.
processJournal ::
  MonadError (Gains.GainsKeeperError Action) m => Journal -> m Journal
processJournal = fmap snd . processJournalWithChanges

processJournalWithChanges ::
  MonadError (Gains.GainsKeeperError Action) m =>
  Journal ->
  m ([Annotated (Gains.Change Action)], Journal)
processJournalWithChanges =
  fmap
    ( \acts ->
        ( acts,
          Journal
            ( concatMap
                ( \x ->
                    case x ^. item of
                      Gains.Result r -> [r <$ x]
                      _ -> []
                )
                acts
            )
        )
    )
    . processActionsWithChanges
    . view actions

processActionsWithChanges ::
  MonadError (Gains.GainsKeeperError Action) m =>
  [Annotated Action] ->
  m [Annotated (Gains.Change Action)]
processActionsWithChanges xs = (`evalStateT` newJournalState) $ do
  fmap concat . forM xs $ \x -> do
    let lot = x ^? item . _Lot
        macct = x ^? details . traverse . _Account
        sym = lot ^. _Just . symbol
    zoom (accounts . at macct . non newAccountState) $ do
      zoom
        ( zipped
            nextId
            (instruments . at sym . non Gains.newInstrumentState)
        )
        $ Gains.processAction x

{-
checkNetAmount ::
  MonadError GainsKeeperError m =>
  Annotated Action ->
  m (Annotated Action)
checkNetAmount x
  | Just itemNet <- x ^? item . _Lot . details . traverse . _Net = do
    let calcNet = netAmount (x ^. item)
    if itemNet == calcNet
      then pure x
      else throwError $ NetAmountDoesNotMatch x calcNet itemNet
  | otherwise =
    pure $ x & item . _Lot . details <>~ [Net (netAmount (x ^. item))]

checkBalance ::
  MonadError GainsKeeperError m =>
  Annotated Action ->
  StateT (Amount 2) m (Annotated Action)
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
-}
