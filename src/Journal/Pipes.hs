{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Pipes where

import Control.Lens
import Control.Monad.Except
import Data.Text (Text)
import qualified Journal.GainsKeeper as Gains
import Journal.Print
import Journal.Types
import Pipes

instance Gains.ActionLike (Either Event Action) where
  _Buy = _Right . _Buy
  _Sell = _Right . _Sell
  _Open = _Left . _Open
  _Close = _Left . _Close
  _Wash = _Left . _Wash

processActions ::
  (MonadError (Gains.GainsKeeperError (Either Event Action)) m, MonadIO m) =>
  Pipe (Annotated (Either Event Action)) Text m r
processActions =
  Gains.gainsKeeper
    >-> Gains.pickResults
    >-> printActions

handleActions ::
  (MonadFail m, MonadIO m) =>
  Producer (Annotated (Either Event Action)) m r ->
  Pipe
    (Annotated (Either Event Action))
    Text
    (ExceptT (Gains.GainsKeeperError (Either Event Action)) m)
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
  Producer (Annotated (Either Event Action)) m r ->
  Consumer Text m r ->
  m r
parseProcessPrint actions = handleActions actions processActions
