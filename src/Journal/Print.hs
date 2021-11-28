{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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

printActions :: MonadIO m => Pipe (Annotated (Either Event Action)) T.Text m r
printActions = forever $ do
  x <- await
  yield $
    TL.toStrict $
      ( case x ^? time of
          Just t -> printTime t <> " "
          Nothing -> ""
      )
        <> either printEvent printAction (x ^. item)
        <> case printAnnotated
          x
          (x ^? item . failing (_Left . _EventLot) (_Right . _Lot)) of
          "" -> ""
          anns -> " " <> anns

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printAction :: Action -> Text
printAction = \case
  Deposit amt -> "deposit " <> printAmount 2 amt
  Withdraw amt -> "withdraw " <> printAmount 2 amt
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot -> "xferin " <> printLot lot
  TransferOut lot -> "xferout " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot

printEvent :: Event -> Text
printEvent = \case
  Open disp lot -> "open " <> printDisposition disp <> " " <> printLot lot
  Close disp lot pl ->
    "close "
      <> printDisposition disp
      <> " "
      <> printLot lot
      <> " "
      <> (if pl < 0 then "loss" else "gain")
      <> " "
      <> totalAmount (Just lot) 2 (abs pl)
  Wash period moment lot ->
    "wash "
      <> printPeriod period
      <> " "
      <> printTime moment
      <> " "
      <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount 2 amt <> " " <> printLot lot
  Interest amt Nothing -> "interest " <> printAmount 2 amt
  Interest amt (Just sym) ->
    "interest " <> printAmount 2 amt <> " from " <> printString sym
  Income amt -> "income " <> printAmount 2 amt
  Credit amt -> "credit " <> printAmount 2 amt
  where
    printDisposition Long = "long"
    printDisposition Short = "short"

    printPeriod Past = "past"
    printPeriod Present = "present"
    printPeriod Future = "future"

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
      Washed x -> Just $ Right $ "washed " <> totalAmount mlot 6 x
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
      Idents xs -> Just $ Left $ "ids " <> TL.pack (show xs)
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
