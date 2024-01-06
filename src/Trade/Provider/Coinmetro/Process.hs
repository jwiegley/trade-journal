{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Trade.Provider.Coinmetro.Process where

import Amount
import Control.Lens hiding (Context)
import Data.Foldable
import Data.Text qualified as T
import Trade.Journal.Entry qualified as Journal
import Trade.Journal.Types
import Trade.Provider.Coinmetro.Types

xactAction ::
  Context ->
  Transaction ->
  Amount 2 ->
  [Annotated Journal.Entry]
xactAction ctx xact _bal
  | "Order" `T.isInfixOf` (xact ^. xactDescription),
    xact ^. xactAsset == ctx ^. currency =
      undefined
  | "Deposit" `T.isInfixOf` (xact ^. xactDescription) = undefined
  | "Withdrawal" `T.isInfixOf` (xact ^. xactDescription) = undefined
  | otherwise = []

coinmetroEntries ::
  Context ->
  [Transaction] ->
  [Annotated Journal.Entry]
coinmetroEntries ctx cmXacts =
  snd $
    (\f -> foldr' f (0 :: Amount 2, []) cmXacts) $
      \xact (bal, rest) ->
        let bal' = bal + xact ^. xactAmount
         in (bal', xactAction ctx xact bal' ++ rest)
