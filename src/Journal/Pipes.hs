{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Journal.Pipes where

import Control.Monad.Except
import Data.Sum
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Journal.Print
import Data.Sum.Lens
import Journal.Types

processEntries ::
  (HasTraversal' HasLot s, Apply Printable s) =>
  ( [Annotated (Sum r v)] ->
    [Annotated (Sum s v)]
  ) ->
  [Annotated (Sum r v)] ->
  [Text]
processEntries handleData = map TL.toStrict . printEntries . handleData

parseProcessPrint ::
  ( HasTraversal' HasLot s,
    Apply Printable s,
    MonadFail m,
    MonadIO m
  ) =>
  ( [Annotated (Sum r v)] ->
    [Annotated (Sum s v)]
  ) ->
  [Annotated (Sum r v)] ->
  ([Text] -> m ()) ->
  m ()
parseProcessPrint handleData entries printer =
  printer (processEntries handleData entries)
