{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse (parseJournal, printLot) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.List (intersperse)
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

-- 2020-06-09 buy 50 AAPL $316.00

skipLineComment' :: Tokens Text -> Parser ()
skipLineComment' prefix =
  string prefix
    *> void (takeWhileP (Just "character") (\x -> x /= '\n' && x /= '\r'))

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = skipLineComment' "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

keyword :: Text -> Parser Text
keyword = lexeme . string

parseJournal :: Parser [Action Lot]
parseJournal = many parseAction

parseAction :: Parser (Action Lot)
parseAction = do
  _time <- Journal.Parse.parseTime
  kw <- parseKeyword
  _amount <- parseAmount
  _symbol <- TL.toStrict <$> parseSymbol
  _price <- parseAmount
  let _details = []
  pure $ case kw of
    "buy" -> BuySell Lot {..}
    "sell" -> BuySell $ Lot {..} & amount %~ negate
    "adjust" -> BuySell Lot {..}
    _ -> error $ "Unexpected action: " ++ TL.unpack kw

printLot :: Lot -> Text
printLot lot =
  TL.concat $ intersperse " " $
    [ printTime (lot ^. time),
      printAmount (lot ^. amount),
      TL.fromStrict (lot ^. symbol),
      printAmount (lot ^. price)
    ]
      ++ map f (lot ^. details)
  where
    f = \case
      Fees x -> "fees " <> printAmount x
      Commission x -> "commission " <> printAmount x
      GainLoss x
        | x < 0 -> "loss " <> printAmount x
        | otherwise -> "gain " <> printAmount x
      ToBeWashed x -> "toWash " <> printAmount x
      WashSaleAdjust r x -> "washSale " <> case r of
        WashPending -> "pending " <> printAmount x
        WashRetroactive -> "retroactive " <> printAmount x
        WashOnOpen -> "onOpen " <> printAmount x
      PartsWashed -> "washed"
      Position eff -> case eff of
        Open -> "open"
        Close -> "close"

parseKeyword :: Parser Text
parseKeyword =
  keyword "buy"
    <|> keyword "sell"
    <|> keyword "adjust"
    <|> keyword "deposit"
    <|> keyword "withdraw"
    <|> keyword "assign"
    <|> keyword "expire"
    <|> keyword "dividend"

{-
printKeyword :: Action Lot -> Text
printKeyword = \case
  BuySell a
    | a ^. amount < 0 -> "sell"
    | otherwise -> "buy"
  AdjustCostBasis _ -> "adjust"
  DepositWithdraw a
    | a ^. amount < 0 -> "withdraw"
    | otherwise -> "deposit"
  Assign _ -> "assign"
  Expire _ -> "expire"
  Dividend _ -> "dividend"
-}

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

printAmount :: Amount 6 -> Text
printAmount = TL.pack . showAmount 6

parseSymbol :: Parser Text
parseSymbol =
  TL.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace
