{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Journal.Pipes where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Journal.Print
import Journal.Types
import Control.Monad.IO.Class

processEntries ::
  (HasLot a, Printable a) =>
  ( [Annotated a] ->
    [Annotated a]
  ) ->
  [Annotated a] ->
  [Text]
processEntries handleData = map TL.toStrict . printEntries . handleData

parseProcessPrint ::
  ( HasLot a,
    Printable a,
    MonadFail m,
    MonadIO m
  ) =>
  ( [Annotated a] ->
    [Annotated a]
  ) ->
  [Annotated a] ->
  ([Text] -> m ()) ->
  m ()
parseProcessPrint handleData entries printer =
  printer (processEntries handleData entries)
