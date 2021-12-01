{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Pipes where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Journal.Closings as Closings
import Journal.Print
import Journal.Types

processActions ::
  (Monoid a, Eq a) =>
  Closings.Calculation ->
  ([Annotated (Entry a)] -> [Annotated (Entry a)]) ->
  (a -> TL.Text) ->
  [Annotated (Entry a)] ->
  [Text]
processActions mode handleData printData =
  printActions printData . handleData . fst . Closings.closings mode

parseProcessPrint ::
  (Monoid a, Eq a, MonadFail m, MonadIO m) =>
  Closings.Calculation ->
  ([Annotated (Entry a)] -> [Annotated (Entry a)]) ->
  (a -> TL.Text) ->
  [Annotated (Entry a)] ->
  ([Text] -> m ()) ->
  m ()
parseProcessPrint mode handleData printData entries printer =
  printer (processActions mode handleData printData entries)
