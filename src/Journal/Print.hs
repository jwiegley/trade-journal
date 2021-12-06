{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Print where

import Amount
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
import Journal.Types

printActions :: (a -> Text) -> [Annotated (Entry a)] -> [T.Text]
printActions printData = map $ \x ->
  TL.toStrict $
    ( case x ^? time of
        Just t -> printTime t <> " "
        Nothing -> ""
    )
      <> printEntry printData (x ^. item)
      <> case printAnnotated x (x ^? item . _Lot) of
        "" -> ""
        anns -> " " <> anns

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printEntry :: (a -> Text) -> Entry a -> Text
printEntry printData (Entry ev) = printEvent printData ev

printPosition :: (a -> Text) -> Position a -> Text
printPosition printData Position {..} =
  TL.pack (show _posIdent)
    <> " "
    <> printLot _posLot
    <> " "
    <> printDisposition _posDisp
    <> " "
    <> printData _posData
  where
    printDisposition Long = "long"
    printDisposition Short = "short"

printClosing :: (a -> Text) -> Closing a -> Text
printClosing printData Closing {..} =
  TL.concat $
    intersperse
      " "
      [ TL.pack (show _closingIdent),
        printLot _closingLot,
        printData _closingData
      ]

printEvent :: (a -> Text) -> Event a -> Text
printEvent printData = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Withdraw amt -> "withdraw " <> printAmount 2 amt
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot -> "xferin " <> printLot lot
  TransferOut lot -> "xferout " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot
  Open pos -> "open " <> printPosition printData pos
  Close cl -> "close " <> printClosing printData cl
  Assign lot -> "assign " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt Nothing -> "interest " <> printAmount 2 amt
  Interest amt (Just sym) ->
    "interest " <> printAmount 2 amt <> " from " <> printString sym
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

totalAmount :: forall n. KnownNat n => Maybe Lot -> Int -> Amount n -> Text
totalAmount mlot n x =
  printAmount n (totaled (fromMaybe (error "Unexpected") mlot) x)

printAnnotated :: Annotated a -> Maybe Lot -> Text
printAnnotated ann mlot =
  TL.concat
    ( intersperse
        " "
        inlineAnns
    )
    <> case separateAnns of
      [] -> ""
      xs -> TL.concat $ intersperse "\n  " ("" : xs)
  where
    annotations = mapMaybe printAnnotation (sort (ann ^. details))
    (inlineAnns, separateAnns) = partitionEithers annotations
    printAnnotation = \case
      Fees x -> Just $ Left $ "fees " <> totalAmount mlot 2 x
      Commission x -> Just $ Left $ "commission " <> totalAmount mlot 2 x
      Account x -> Just $ Left $ "account " <> printText x
      Ident x -> Just $ Left $ "id " <> TL.pack (show x)
      Order x -> Just $ Left $ "order " <> printText x
      Strategy x -> Just $ Left $ "strategy " <> printText x
      Note x -> Just $ Left $ "note " <> printString x
      Meta k v -> Just $ Right $ "meta " <> printText k <> " " <> printText v
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

printAmount :: forall n. KnownNat n => Int -> Amount n -> Text
printAmount n = TL.pack . amountToString n
