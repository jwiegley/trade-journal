{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Journal.Print where

import Amount
import Control.Lens hiding (noneOf)
import Data.Char
import Data.Either
import Data.List (intersperse, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sum
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.TypeLits
import Journal.SumLens
import Journal.Types

class Printable f where
  printItem :: f v -> Text

instance Apply Printable fs => Printable (Sum fs) where
  printItem s = s ^. applied @Printable printItem

printEntries ::
  (HasTraversal' HasLot r, Apply Printable r) =>
  [Annotated (Sum r v)] ->
  [Text]
printEntries = map $ \x ->
  ( case x ^? time of
      Just t -> printTime t <> " "
      Nothing -> ""
  )
    <> printItem (x ^. item)
    <> case printAnnotated x (x ^? item . traversing @HasLot _Lot) of
      "" -> ""
      anns -> " " <> anns

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

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
printAnnotated ann _mlot =
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
      Note x -> Just $ Left $ "note " <> printString x
      Meta k v -> Just $ Right $ "meta " <> printText k <> " " <> printText v
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

printAmount :: forall n. KnownNat n => Int -> Amount n -> Text
printAmount n = TL.pack . amountToString n
