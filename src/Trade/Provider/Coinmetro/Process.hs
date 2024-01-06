{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Trade.Provider.Coinmetro.Process where

import Amount
import Control.Lens hiding (Context)
import Data.Foldable
import Debug.Trace
import Trade.Journal.Entry qualified as Journal
import Trade.Journal.Types
import Trade.Provider.Coinmetro.Types

xactAction ::
  Context ->
  Transaction ->
  Amount 2 ->
  Either String (Annotated Journal.Entry)
xactAction _ctx _xact _bal = undefined

coinmetroEntries ::
  Context ->
  [Transaction] ->
  [Annotated Journal.Entry]
coinmetroEntries ctx cmXacts =
  concatMap
    ( \case
        Left err -> trace err []
        Right x -> [x]
    )
    $ snd
    $ (\f -> foldr' f (0 :: Amount 2, []) cmXacts)
    $ \xact (bal, rest) ->
      let nxt = bal + xact ^. xactAmount
       in case xactAction ctx xact nxt of
            x@(Left _) -> (bal, x : rest)
            x -> (nxt, x : rest)
