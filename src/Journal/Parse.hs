{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse (parseJournal, printJournal) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.List (intersperse, sort)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Void
import GHC.TypeLits
import Journal.Amount
import Journal.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

skipLineComment' :: Tokens Text -> Parser ()
skipLineComment' prefix =
  string prefix
    *> void (takeWhileP (Just "character") (\x -> x /= '\n' && x /= '\r'))

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = skipLineComment' "|"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

keyword :: Text -> Parser Text
keyword = lexeme . string

parseJournal :: Parser Journal
parseJournal =
  Journal <$> many (whiteSpace *> parseTimed parseAction) <* eof

parseTimed :: Parser a -> Parser (Timed a)
parseTimed p = do
  _time <- Journal.Parse.parseTime
  _item <- p
  pure Timed {..}

parseAction :: Parser Action
parseAction =
  keyword "buy" *> (Buy <$> parseLot)
    <|> keyword "sell" *> (Sell <$> parseLot)
    <|> keyword "wash" *> (Wash <$> parseLot)
    <|> keyword "deposit" *> (Deposit <$> parseLot)
    <|> keyword "withdraw" *> (Withdraw <$> parseLot)
    <|> keyword "assign" *> (Assign <$> parseLot)
    <|> keyword "expire" *> (Expire <$> parseLot)
    <|> keyword "dividend" *> (Dividend <$> parseLot)

parseLot :: Parser Lot
parseLot = do
  _amount <- parseAmount
  _symbol <- TL.toStrict <$> parseSymbol
  _price <- parseAmount
  _details <- many parseAnnotation
  pure $
    Lot {..}
      & details . traverse . failing _Fees _Commission //~ _amount

parseAnnotation :: Parser Annotation
parseAnnotation = do
  keyword "position" *> (Position <$> parseEffect)
    <|> keyword "fees" *> (Fees <$> parseAmount)
    <|> keyword "commission" *> (Commission <$> parseAmount)
    <|> keyword "gain" *> (Gain <$> parseAmount)
    <|> keyword "loss" *> (Loss <$> parseAmount)
    <|> keyword "washed" *> (Washed <$> parseAmount)
    <|> keyword "wash" *> parseWash
    <|> keyword "apply"
      *> (WashApply . TL.toStrict <$> parseSymbol <*> parseAmount)
    <|> keyword "exempt" *> pure Exempt
    <|> keyword "balance" *> (Balance <$> parseAmount)
    <|> keyword "account" *> (Account <$> parseText)
    <|> keyword "trade" *> (Trade <$> parseText)
    <|> keyword "order" *> (Order <$> parseText)
    <|> keyword "xact" *> (Transaction <$> parseText)
    <|> keyword "meta" *> (Meta <$> parseText <*> parseText)
  where
    parseWash =
      WashTo Nothing Nothing <$ keyword "dropped"
        <|> do
          mres <- optional $ do
            q <- parseAmount
            _ <- char '@' <* whiteSpace
            p <- parseAmount
            pure (q, p)
          _ <- keyword "to"
          sym <- parseSymbol
          pure $ WashTo (Just (TL.toStrict sym)) mres
    parseEffect =
      Open <$ keyword "open"
        <|> Close <$ keyword "close"

printJournal :: Bool -> Journal -> Text
printJournal b =
  TL.concat
    . intersperse "\n"
    . map (printTimed (printAction b))
    . view actions

printTimed :: (a -> Text) -> Timed a -> Text
printTimed printItem t =
  TL.concat $ intersperse " " $
    [ printTime (t ^. time),
      printItem (t ^. item)
    ]

printAction :: Bool -> Action -> Text
printAction b = \case
  Buy lot -> "buy " <> printLot b lot
  Sell lot -> "sell " <> printLot b lot
  Wash lot -> "wash " <> printLot b lot
  Deposit lot -> "deposit " <> printLot b lot
  Withdraw lot -> "withdraw " <> printLot b lot
  Assign lot -> "assign " <> printLot b lot
  Expire lot -> "expire " <> printLot b lot
  Dividend lot -> "dividend " <> printLot b lot

printLot :: Bool -> Lot -> Text
printLot b lot
  | not b = basic
  | otherwise =
    basic
      <> "\n  total "
      <> printAmount 4 (lot ^. amount . coerced * lot ^. price)
      <> " "
      <> ( TL.concat
             $ intersperse " "
             $ map
               (printAnnotationSum (lot ^. amount . coerced))
               (sort (lot ^. details))
         )
  where
    basic =
      TL.concat $ intersperse " " $
        [ printAmount 0 (lot ^. amount),
          TL.fromStrict (lot ^. symbol),
          printAmount 4 (lot ^. price)
        ]
          ++ map printAnnotation (sort (lot ^. details))
    printAnnotation = \case
      Position eff -> case eff of
        Open -> "open"
        Close -> "close"
      Fees x -> "fees " <> printAmount 2 x
      Commission x -> "commission " <> printAmount 2 x
      Gain x -> "gain " <> printAmount 6 x
      Loss x -> "loss " <> printAmount 6 x
      Washed amt -> "washed " <> printAmount 6 amt
      WashTo Nothing _ -> "wash dropped"
      WashTo (Just x) (Just (q, p)) ->
        "wash "
          <> printAmount 0 q
          <> " @ "
          <> printAmount 4 p
          <> " to "
          <> TL.fromStrict x
      WashTo (Just x) Nothing -> "wash to " <> TL.fromStrict x
      WashApply x amt -> "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
      Exempt -> "exempt"
      Balance x -> "balance " <> printAmount 2 (x ^. coerced)
      Account x -> "account " <> printText x
      Trade x -> "trade " <> printText x
      Order x -> "order " <> printText x
      Transaction x -> "transaction " <> printText x
      Meta k v -> "meta " <> printText k <> " " <> printText v
    printAnnotationSum n = \case
      Fees x ->
        "fees " <> printAmount 2 (x * n)
      Commission x ->
        "commission " <> printAmount 2 (x * n)
      Gain x ->
        "basis " <> printAmount 2 (lot ^. price - x - lotFees lot) <> " "
          <> "gain "
          <> printAmount 6 (x * n)
      Loss x ->
        "basis " <> printAmount 2 (lot ^. price + x - lotFees lot) <> " "
          <> "loss "
          <> printAmount 6 (x * n)
      Washed amt -> "washed " <> printAmount 6 (lot ^. price + amt)
      _ -> ""
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

parseText :: Parser T.Text
parseText =
  T.pack
    <$> ( char '"' *> manyTill L.charLiteral (char '"')
            <|> some (satisfy isAlphaNum)
        )

parseTime :: Parser UTCTime
parseTime = do
  dateString <- some (satisfy (\c -> isDigit c || c == '-')) <* whiteSpace
  timeString <-
    optional (some (satisfy (\c -> isDigit c || c == ':')) <* whiteSpace)
  case timeString of
    Nothing -> parseTimeM False defaultTimeLocale "%Y-%m-%d" dateString
    Just str ->
      parseTimeM
        False
        defaultTimeLocale
        "%Y-%m-%d %H:%M:%S%Q"
        (dateString ++ " " ++ str)

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

parseAmount :: KnownNat n => Parser (Amount n)
parseAmount =
  read <$> some (satisfy (\c -> isDigit c || c == '.')) <* whiteSpace

printAmount :: Int -> Amount 6 -> Text
printAmount n = TL.pack . amountToString n

parseSymbol :: Parser Text
parseSymbol =
  TL.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace
