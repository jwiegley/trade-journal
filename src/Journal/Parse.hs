{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse (parseJournal, printJournal) where

import Control.Lens
import Data.Char
import Data.List (intersperse, sort)
import Data.Maybe
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
    *> takeWhileP (Just "character") (\x -> x /= '\n' && x /= '\r')
    *> pure ()

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
    <|> keyword "net" *> (Net <$> parseAmount)
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
  Deposit lot -> "deposit " <> printLot lot
  Withdraw lot -> "withdraw " <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend lot -> "dividend " <> printLot lot

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
    annotations = map printAnnotation (sort (lot ^. details))
    inlineAnns = mapMaybe (either Just (const Nothing)) annotations
    separateAnns = mapMaybe (either (const Nothing) Just) annotations
    printAnnotation = \case
      Position eff -> Right $ case eff of
        Open -> "open"
        Close -> "close"
      Fees x -> Left $ "fees " <> printAmount 2 (totaled lot x)
      Commission x -> Left $ "commission " <> printAmount 2 (totaled lot x)
      Gain x -> Right $ "gain " <> printAmount 6 (totaled lot x)
      Loss x -> Right $ "loss " <> printAmount 6 (totaled lot x)
      Washed x -> Right $ "washed " <> printAmount 6 (totaled lot x)
      WashTo Nothing _ -> Right $ "wash dropped"
      WashTo (Just x) (Just (q, p)) ->
        Right $
          "wash "
            <> printAmount 0 q
            <> " @ "
            <> printAmount 4 p
            <> " to "
            <> TL.fromStrict x
      WashTo (Just x) Nothing -> Left $ "wash to " <> TL.fromStrict x
      WashApply x amt ->
        Left $ "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
      Exempt -> Left "exempt"
      Net x -> Right $ "net " <> printAmount 2 (x ^. coerced)
      Balance x -> Right $ "balance " <> printAmount 2 (x ^. coerced)
      Account x -> Left $ "account " <> printText x
      Trade x -> Left $ "trade " <> printText x
      Order x -> Left $ "order " <> printText x
      Transaction x -> Left $ "transaction " <> printText x
      Meta k v -> Right $ "meta " <> printText k <> " " <> printText v
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
