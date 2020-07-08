{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse (parseJournal, printJournal) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.List (intersperse, sort)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Void
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
parseAction = do
  kw <- parseKeyword
  case kw of
    "buy" -> Buy <$> parseLot
    "sell" -> Sell <$> parseLot
    "wash" -> Wash <$> parseLot
    "deposit" -> Deposit <$> parseLot
    "withdraw" -> Withdraw <$> parseLot
    "assign" -> Assign <$> parseLot
    "expire" -> Expire <$> parseLot
    "dividend" -> Dividend <$> parseLot
    _ -> error $ "Unexpected action: " ++ TL.unpack kw

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
  keyword "fees" *> (Fees <$> parseAmount)
    <|> keyword "commission" *> (Commission <$> parseAmount)
    <|> keyword "gain" *> (Gain <$> parseAmount)
    <|> keyword "loss" *> (Loss <$> parseAmount)
    <|> keyword "washed" *> (Washed <$> parseAmount)
    <|> keyword "wash" *> parseWash
    <|> keyword "apply"
      *> (WashApply . TL.toStrict <$> parseSymbol <*> parseAmount)
    <|> keyword "exempt" *> pure Exempt
    <|> keyword "position" *> (Position <$> parseEffect)
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

printJournal :: Journal -> Text
printJournal =
  TL.concat
    . intersperse "\n"
    . map (printTimed printAction)
    . view actions

printTimed :: (a -> Text) -> Timed a -> Text
printTimed printItem t =
  TL.concat $ intersperse " " $
    [ printTime (t ^. time),
      printItem (t ^. item)
    ]

printAction :: Action -> Text
printAction = \case
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  Wash lot -> "wash " <> printLot lot
  WashDropped lot -> "dropped " <> printLot lot
  Deposit lot -> "deposit " <> printLot lot
  Withdraw lot -> "withdraw " <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend lot -> "dividend " <> printLot lot

printLot :: Lot -> Text
printLot lot =
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
      Trade x -> "trade " <> TL.fromStrict x
      Order x -> "order " <> TL.fromStrict x
      Transaction x -> "transaction " <> TL.fromStrict x
      Account x -> "account " <> TL.fromStrict x
      Position eff -> case eff of
        Open -> "open"
        Close -> "close"
      Balance x -> "balance " <> printAmount 2 (x ^. coerced)
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

parseKeyword :: Parser Text
parseKeyword =
  keyword "buy"
    <|> keyword "sell"
    <|> keyword "wash"
    <|> keyword "deposit"
    <|> keyword "withdraw"
    <|> keyword "assign"
    <|> keyword "expire"
    <|> keyword "dividend"

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

parseAmount :: Parser (Amount 6)
parseAmount =
  read <$> some (satisfy (\c -> isDigit c || c == '.')) <* whiteSpace

printAmount :: Int -> Amount 6 -> Text
printAmount n = TL.pack . amountToString n

parseSymbol :: Parser Text
parseSymbol =
  TL.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace
