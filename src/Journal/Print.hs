{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Journal.Print (printJournal) where

import Control.Lens hiding (noneOf)
import Data.Char
import Data.Either
import Data.List (intersperse, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.TypeLits
import Journal.Amount
import Journal.Model
import Journal.Types

printJournal :: Journal -> Text
printJournal =
  TL.concat
    . intersperse "\n"
    . map (printTimed printAction)
    . view actions

printTimed :: (a -> Text) -> Timed a -> Text
printTimed printItem t =
  TL.concat $
    intersperse " " $
      [ printTime (t ^. time),
        printItem (t ^. item)
      ]

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printAction :: Action -> Text
printAction = \case
  Deposit amt desc -> "deposit " <> printAmount 2 amt <> " " <> printString desc
  Withdraw amt desc -> "withdraw " <> printAmount 2 amt <> " " <> printString desc
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot desc -> "in " <> printLot lot <> " " <> TL.fromStrict desc
  TransferOut lot desc -> "out " <> printLot lot <> " " <> TL.fromStrict desc
  Wash lot -> "wash " <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt desc -> "interest " <> printAmount 2 amt <> " " <> printString desc
  Income amt desc -> "income " <> printAmount 2 amt <> " " <> printString desc
  Credit amt desc -> "credit " <> printAmount 2 amt <> " " <> printString desc

printLot :: Lot -> Text
printLot lot =
  ( TL.concat $
      intersperse
        " "
        ( [ printAmount 0 (lot ^. amount),
            TL.fromStrict (lot ^. symbol),
            printAmount 4 (lot ^. price)
          ]
            ++ inlineAnns
        )
  )
    <> case separateAnns of
      [] -> ""
      xs ->
        ( TL.concat $
            intersperse "\n  " ("" : xs)
        )
  where
    annotations =
      mapMaybe
        printAnnotation
        (sort (lot ^. details ++ lot ^. computed))
    (inlineAnns, separateAnns) = partitionEithers annotations
    totalAmount :: forall n. KnownNat n => Int -> Amount n -> Text
    totalAmount n x = printAmount n (totaled lot x)
    printAnnotation = \case
      Position eff -> Just $
        Left $ case eff of
          Open -> "open"
          Close -> "close"
      Fees x -> Just $ Left $ "fees " <> totalAmount 2 x
      Commission x -> Just $ Left $ "commission " <> totalAmount 2 x
      Gain x -> Just $ Right $ "gain " <> totalAmount 6 x
      Loss x -> Just $ Right $ "loss " <> totalAmount 6 x
      Washed x -> Just $ Right $ "washed " <> totalAmount 6 x
      WashTo x (Just (q, p)) ->
        Just $
          Left $
            "wash "
              <> printAmount 0 q
              <> " @ "
              <> printAmount 4 p
              <> " to "
              <> TL.fromStrict x
      WashTo x Nothing -> Just $ Left $ "wash to " <> TL.fromStrict x
      WashApply x amt ->
        Just $ Left $ "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
      Exempt -> Just $ Left "exempt"
      Net _x -> Nothing
      Balance _x -> Nothing
      -- Net x -> Right $ "net " <> printAmount 2 (x ^. coerced)
      -- Balance x -> Right $ "balance " <> printAmount 2 (x ^. coerced)
      Account x -> Just $ Left $ "account " <> printText x
      Trade x -> Just $ Left $ "trade " <> printText x
      Order x -> Just $ Left $ "order " <> printText x
      Transaction x -> Just $ Left $ "transaction " <> printText x
      Meta k v -> Just $ Right $ "meta " <> printText k <> " " <> printText v
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

printAmount :: forall n. KnownNat n => Int -> Amount n -> Text
printAmount n = TL.pack . amountToString n
