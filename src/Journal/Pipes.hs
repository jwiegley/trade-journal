{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Pipes where

import Control.Monad.Except
import Data.Text (Text)
import qualified Journal.Gains as Gains
import qualified Journal.GainsKeeper as GainsKeeper
import Journal.Print
import Journal.Types
import Pipes

instance Gains.ActionLike Entry where
  _Buy = _Action . _Buy
  _Sell = _Action . _Sell
  _Open = undefined -- _Event . _Open
  _Close = undefined -- _Event . _Close

instance GainsKeeper.ActionLike Entry where
  _Buy = _Action . _Buy
  _Sell = _Action . _Sell
  _Open = undefined -- _Event . _Open
  _Close = undefined -- _Event . _Close
  _Wash = _Event . _Wash

processActions ::
  (MonadError (GainsKeeper.GainsKeeperError Entry) m, MonadIO m) =>
  Pipe (Annotated Entry) Text m r
processActions =
  GainsKeeper.gainsKeeper
    >-> GainsKeeper.pickResults
    >-> printActions

handleActions ::
  (MonadFail m, MonadIO m) =>
  Producer (Annotated Entry) m r ->
  Pipe
    (Annotated Entry)
    Text
    (ExceptT (GainsKeeper.GainsKeeperError Entry) m)
    r ->
  Consumer Text m r ->
  m r
handleActions actions pipe printer = do
  eres <-
    runExceptT $
      runEffect $
        hoist lift actions
          >-> pipe
          >-> hoist lift printer
  case eres of
    Left err -> fail $ "Error " ++ show err
    Right res -> pure res

parseProcessPrint ::
  (MonadFail m, MonadIO m) =>
  Producer (Annotated Entry) m r ->
  Consumer Text m r ->
  m r
parseProcessPrint actions = handleActions actions processActions
