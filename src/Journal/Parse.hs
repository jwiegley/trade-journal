{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Parse where

import Amount
import Control.Lens hiding (each, noneOf)
import Control.Monad.IO.Class
import Data.Char
import Data.Functor
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time hiding (parseTime)
import Data.Void
import GHC.TypeLits
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
    ( many (whiteSpace *> parseAnnotatedEntry)
        <* eof
    )
    path
    input of
    Left e -> fail $ errorBundlePretty e
    Right res -> pure res

parseAnnotatedEntry :: Parser (Annotated Entry)
parseAnnotatedEntry = do
  _time <- Journal.Parse.parseTime
  _item <- parseEntry
  _details <- many parseAnnotation
  -- if there are fees, there should be an amount
  pure $
    Annotated {..}
      & details . traverse . failing _Fees _Commission
        //~ (_item ^?! _EntryLot . amount)

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

parseEntry :: Parser Entry
parseEntry =
  keyword "deposit" *> (Deposit <$> parseAmount)
    <|> keyword "withdraw" *> (Withdraw <$> parseAmount)
    <|> keyword "buy" *> (Buy <$> parseLot)
    <|> keyword "sell" *> (Sell <$> parseLot)
    <|> keyword "xferin" *> (TransferIn <$> parseLot)
    <|> keyword "xferout" *> (TransferOut <$> parseLot)
    <|> keyword "exercise" *> (Exercise <$> parseLot)
    -- <|> keyword "open" *> (Open <$> parsePosition parseData)
    -- <|> keyword "close" *> (Close <$> parseClosing parseData)
    <|> keyword "assign" *> (Assign <$> parseLot)
    <|> keyword "expire" *> (Expire <$> parseLot)
    <|> keyword "dividend" *> (Dividend <$> parseAmount <*> parseLot)
    <|> keyword "interest"
      *> ( Interest <$> parseAmount
             <*> optional (keyword "from" *> (TL.toStrict <$> parseSymbol))
         )
    <|> keyword "income" *> (Income <$> parseAmount)
    <|> keyword "credit" *> (Credit <$> parseAmount)

parseLot :: Parser Lot
parseLot = do
  _amount <- parseAmount
  _symbol <- TL.toStrict <$> parseSymbol
  _price <- parseAmount
  pure Lot {..}

parseAnnotation :: Parser Annotation
parseAnnotation = do
  keyword "fees" *> (Fees <$> parseAmount)
    <|> keyword "commission" *> (Commission <$> parseAmount)
    <|> keyword "account" *> (Account <$> parseText)
    <|> keyword "id" *> (Ident <$> L.decimal)
    <|> keyword "order" *> (Order <$> parseText)
    <|> keyword "strategy" *> (Strategy <$> parseText)
    <|> keyword "note" *> (Note <$> quotedString)
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
  TL.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace
