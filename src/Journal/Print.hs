{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Print (printActions) where

import Amount
import Control.Lens hiding (noneOf)
import Control.Monad
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
import Pipes

printActions :: MonadIO m => Pipe (Annotated Entry) T.Text m r
printActions = forever $ do
  x <- await
  yield $
    TL.toStrict $
      ( case x ^? time of
          Just t -> printTime t <> " "
          Nothing -> ""
      )
        <> printEntry (x ^. item)
        <> case printAnnotated x (x ^? item . _Lot) of
          "" -> ""
          anns -> " " <> anns

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printEntry :: Entry -> Text
printEntry (Action act) = printAction act
printEntry (Event ev) = printEvent ev

printAction :: Action -> Text
printAction = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Withdraw amt -> "withdraw " <> printAmount 2 amt
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot -> "xferin " <> printLot lot
  TransferOut lot -> "xferout " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot

printPosition :: Position -> Text
printPosition Position {..} =
  TL.pack (show _posIdent)
    <> " "
    <> printLot _posLot
    <> " "
    <> printDisposition _posDisp
    <> " "
    <> printAmount 2 _posBasis
  where
    printDisposition Long = "long"
    printDisposition Short = "short"

printClosing :: Closing -> Text
printClosing Closing {..} =
  "(" <> printPosition _closingPos
    <> ") "
    <> printLot _closingLot

printEvent :: Event -> Text
printEvent = \case
  Open pos -> "open " <> printPosition pos
  Close cl -> "close " <> printClosing cl
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

printWashing :: Washing -> Either Text Text
printWashing = \case
  WashedFromPast x _ -> Right $ "washed from past " <> printAmount 2 x
  WashedFromFuture x _ -> Right $ "washed from future " <> printAmount 2 x
  WashTo x (Just (q, p)) ->
    Left $
      "wash "
        <> printAmount 0 q
        <> " @ "
        <> printAmount 4 p
        <> " to "
        <> TL.fromStrict x
  WashTo x Nothing -> Left $ "wash to " <> TL.fromStrict x
  WashApply x amt ->
    Left $ "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
  Exempt -> Left "exempt"

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
