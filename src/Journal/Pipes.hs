{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Pipes where

import Control.Monad.Except
import Data.Text (Text)
import qualified Journal.GainsKeeper as Gains
import Journal.Print
import Journal.Types
import Pipes

instance Gains.ActionLike Action where
  _Buy = _Buy
  _Sell = _Sell
  _Wash = _Wash

processActions ::
  (MonadError (Gains.GainsKeeperError Action) m, MonadIO m) =>
  Pipe (Annotated Action) Text m r
processActions =
  Gains.gainsKeeper
    >-> Gains.pickResults
    >-> printActions

handleActions ::
  (MonadFail m, MonadIO m) =>
  Producer (Annotated Action) m r ->
  Pipe
    (Annotated Action)
    Text
    (ExceptT (Gains.GainsKeeperError Action) m)
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
  Producer (Annotated Action) m r ->
  Consumer Text m r ->
  m r
parseProcessPrint actions = handleActions actions processActions
