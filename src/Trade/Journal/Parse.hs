{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Trade.Journal.Parse where

import Amount
import Control.Lens hiding (Context, each, noneOf)
import Control.Monad.IO.Class
import Data.Char
import Data.Functor
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time
import Data.Void
import GHC.TypeLits
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Trade.Journal.Types

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

parseJournal :: (MonadFail m, MonadIO m) => FilePath -> m (Journal T.Text)
parseJournal path = do
  input <- liftIO $ TL.readFile path
  parseJournalFromText path input

parseJournalFromText :: (MonadFail m) => FilePath -> Text -> m (Journal T.Text)
parseJournalFromText path input =
  case parse
    ( many
        ( do
            trade <- parseTrade
            skipMany hspace1
            _ <- newline
            pure trade
        )
        <* eof
    )
    path
    input of
    Left e -> fail $ errorBundlePretty e
    Right res -> pure $ Journal res

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

parseText :: Parser T.Text
parseText =
  T.pack
    <$> ( char '"'
            *> manyTill L.charLiteral (char '"')
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

parseAmount :: (KnownNat n) => Parser (Amount n)
parseAmount =
  read <$> some (digitChar <|> char '.') <* skipMany hspace1

parseSymbol :: Parser Text
parseSymbol =
  TL.pack
    <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace

parseLot :: Parser (T.Text, Lot)
parseLot = do
  tm <- Trade.Journal.Parse.parseTime
  action <- (True <$ keyword "buy") <|> (False <$ keyword "sell")
  amt <- (if action then id else negate) <$> parseAmount
  sym <- TL.toStrict <$> parseSymbol
  _ <- char '@' <* whiteSpace
  pr <- parseAmount
  pure (sym, Lot amt (TimePrice pr tm))

parseTrade :: Parser (T.Text, Trade)
parseTrade = do
  (sym, lot) <- parseLot
  fees <- fromMaybe 0 <$> optional (keyword "fees" *> parseAmount)
  commissions <- fromMaybe 0 <$> optional (keyword "commissions" *> parseAmount)
  pure (sym, Trade lot fees commissions)
