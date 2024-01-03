{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Journal.Parse where

import Amount
import Control.Lens hiding (Context, each, noneOf)
import Control.Monad.IO.Class
import Data.Char
import Data.Functor
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time
import Data.Void
import GHC.TypeLits
import Journal.Entry
import Journal.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

skipLineComment' :: Tokens Text -> Parser ()
skipLineComment' prefix =
  string prefix
    *> takeWhileP (Just "character") (\x -> x /= '\n' && x /= '\r')
    $> ()

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = skipLineComment' "|"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

keyword :: Text -> Parser Text
keyword = lexeme . string

parseEntries ::
  (MonadFail m, MonadIO m) =>
  FilePath ->
  m [Annotated Entry]
parseEntries path = do
  input <- liftIO $ TL.readFile path
  parseEntriesFromText path input

parseEntriesFromText ::
  MonadFail m =>
  FilePath ->
  Text ->
  m [Annotated Entry]
parseEntriesFromText path input =
  case parse
    ( many (whiteSpace *> parseAnnotated parseEntry)
        <* eof
    )
    path
    input of
    Left e -> fail $ errorBundlePretty e
    Right res -> pure res

parseAnnotated :: Parser a -> Parser (Annotated a)
parseAnnotated parser = do
  _time <- Journal.Parse.parseTime
  _item <- parser
  _details <- many parseAnnotation
  let _context =
        Context
          { _account = "NYI",
            _currency = "NYI"
          }
  pure Annotated {..}

-- & details . traverse . failing _Fees _Commission
--   //~ (Const _item ^?! _Lot . amount)

quotedString :: Parser T.Text
quotedString = identPQuoted <&> T.pack
  where
    escape :: Parser String
    escape = do
      d <- char '\\'
      c <- oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']
      return [d, c]

    nonEscape :: Parser Char
    nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

    identPQuoted :: Parser String
    identPQuoted =
      let inner = fmap return (try nonEscape) <|> escape
       in do
            _ <- char '"'
            strings <- many inner
            _ <- char '"'
            return $ concat strings

parseLot :: Parser Lot
parseLot = do
  _amount <- parseAmount
  _symbol <- TL.toStrict <$> parseSymbol
  _price <- parseAmount
  pure Lot {..}

parseAnnotation :: Parser Annotation
parseAnnotation = do
  -- keyword "fees" *> (Fees <$> parseAmount)
  --   <|> keyword "commission" *> (Commission <$> parseAmount)
  --   <|> keyword "account" *> (Account <$> parseText)
  --   <|> keyword "id" *> (Ident <$> L.decimal)
  --   <|> keyword "order" *> (Order <$> parseText)
  -- <|> keyword "strategy" *> (Strategy <$> parseText)
  keyword "note" *> (Note <$> quotedString)
    <|> keyword "meta" *> (Meta <$> parseText <*> parseText)

parseText :: Parser T.Text
parseText =
  T.pack
    <$> ( char '"' *> manyTill L.charLiteral (char '"')
            <|> some alphaNumChar
        )

parseTime :: Parser UTCTime
parseTime = do
  dateString <- some (digitChar <|> char '-') <* whiteSpace
  timeString <-
    optional (some (digitChar <|> char ':') <* whiteSpace)
  case timeString of
    Nothing -> parseTimeM False defaultTimeLocale "%Y-%m-%d" dateString
    Just str ->
      parseTimeM
        False
        defaultTimeLocale
        "%Y-%m-%d %H:%M:%S%Q"
        (dateString ++ " " ++ str)

parseAmount :: KnownNat n => Parser (Amount n)
parseAmount =
  read <$> some (digitChar <|> char '.') <* whiteSpace

parseSymbol :: Parser Text
parseSymbol =
  TL.pack
    <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace

parseDeposit :: Parser Deposit
parseDeposit =
  keyword "deposit"
    *> ( Deposit
           <$> parseAmount
           <*> (keyword "from" *> parseText)
       )
    <|> keyword "withdraw"
      *> ( Deposit
             <$> (negate <$> parseAmount)
             <*> (keyword "to" *> parseText)
         )
    <|> keyword "transfer"
      *> ( Transfer
             <$> parseLot
             <*> (keyword "from" *> parseText)
         )
    <|> keyword "transfer"
      *> ( Transfer
             <$> (over amount negate <$> parseLot)
             <*> (keyword "to" *> parseText)
         )

parseTrade :: Parser Trade
parseTrade = do
  _tradeAction <- (Buy <$ keyword "buy") <|> (Sell <$ keyword "sell")
  _tradeLot <- parseLot
  _tradeFees <-
    Fees
      <$> ( ( keyword "fees"
                *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
            )
              <|> pure 0
          )
      <*> ( ( keyword "commission"
                *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
            )
              <|> pure 0
          )
  pure Trade {..}

parseOptions :: Parser Options
parseOptions =
  keyword "exercise" *> (Exercise <$> parseLot)
    <|> keyword "assign" *> (Assign <$> parseLot)
    <|> keyword "expire" *> (Expire <$> parseLot)

parseIncome :: Parser Income
parseIncome =
  keyword "dividend" *> (Dividend <$> parseAmount <*> parseLot)
    <|> keyword "interest"
      *> ( Interest
             <$> parseAmount
             <*> optional (keyword "from" *> (TL.toStrict <$> parseSymbol))
         )
    <|> keyword "income" *> (Income <$> parseAmount)
    <|> keyword "credit" *> (Credit <$> parseAmount)

parseEntry :: Parser Entry
parseEntry =
  (TradeEntry <$> parseTrade)
    <|> (OptionsEntry <$> parseOptions)
    <|> (IncomeEntry <$> parseIncome)
    <|> (DepositEntry <$> parseDeposit)
