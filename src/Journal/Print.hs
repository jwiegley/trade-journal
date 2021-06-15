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
import Data.Maybe (fromMaybe, mapMaybe)
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
    . map
      ( \x ->
          ( case x ^? time of
              Just t -> printTime t <> " "
              Nothing -> ""
          )
            <> printAction (x ^. item)
            <> case printAnnotated x (x ^? item . _Lot) of
              "" -> ""
              anns -> " " <> anns
      )
    . view actions

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printAction :: Action -> Text
printAction = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Withdraw amt -> "withdraw " <> printAmount 2 amt
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot -> "in " <> printLot lot
  TransferOut lot -> "out " <> printLot lot
  Wash lot -> "wash " <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt -> "interest " <> printAmount 2 amt
  Income amt -> "income " <> printAmount 2 amt
  Credit amt -> "credit " <> printAmount 2 amt

printLot :: Lot -> Text
printLot lot =
  TL.concat $
    intersperse
      " "
      [ printAmount 0 (lot ^. amount),
        TL.fromStrict (lot ^. symbol),
        printAmount 4 (lot ^. price)
      ]

printAnnotated :: Annotated a -> Maybe Lot -> Text
printAnnotated ann mlot =
  ( TL.concat $
      intersperse
        " "
        inlineAnns
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
        (sort (ann ^. details ++ ann ^. computed))
    (inlineAnns, separateAnns) = partitionEithers annotations
    totalAmount :: forall n. KnownNat n => Int -> Amount n -> Text
    totalAmount n x = printAmount n (totaled (fromMaybe (error "Unexpected") mlot) x)
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
      Account x -> Just $ Left $ "account " <> printText x
      Order x -> Just $ Left $ "order " <> printText x
      Strategy x -> Just $ Left $ "strategy " <> printText x
      Note x -> Just $ Left $ "note " <> printString x
      Time _ -> Nothing
      Meta k v -> Just $ Right $ "meta " <> printText k <> " " <> printText v
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

printAmount :: forall n. KnownNat n => Int -> Amount n -> Text
printAmount n = TL.pack . amountToString n
