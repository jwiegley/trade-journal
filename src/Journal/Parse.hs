{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Journal.Parse
  ( parseActionsAndEvents,
    parseActionsAndEventsFromText,
  )
where

import Amount
import Control.Lens hiding (each, noneOf)
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
import Pipes
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

parseActionsAndEvents ::
  (MonadFail m, MonadIO m) =>
  FilePath ->
  Producer (Annotated Entry) m ()
parseActionsAndEvents path = do
  input <- liftIO $ TL.readFile path
  parseActionsAndEventsFromText path input

parseActionsAndEventsFromText ::
  MonadFail m =>
  FilePath ->
  Text ->
  Producer (Annotated Entry) m ()
parseActionsAndEventsFromText path input =
  case parse
    ( many (whiteSpace *> parseAnnotatedActionOrEvent)
        <* eof
    )
    path
    input of
    Left e -> fail $ errorBundlePretty e
    Right res -> each res

parseAnnotatedActionOrEvent :: Parser (Annotated Entry)
parseAnnotatedActionOrEvent = do
  _time <- Journal.Parse.parseTime
  _item <- Event <$> parseEvent <|> Action <$> parseAction
  _details <- many parseAnnotation
  -- if there are fees, there should be an amount
  pure $
    Annotated {..}
      & details . traverse . failing _Fees _Commission
        //~ (_item ^?! _Lot . amount)

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

parseAction :: Parser Action
parseAction =
  keyword "deposit" *> (Deposit <$> parseAmount)
    <|> keyword "withdraw" *> (Withdraw <$> parseAmount)
    <|> keyword "buy" *> (Buy <$> parseLot)
    <|> keyword "sell" *> (Sell <$> parseLot)
    <|> keyword "xferin" *> (TransferIn <$> parseLot)
    <|> keyword "xferout" *> (TransferOut <$> parseLot)
    <|> keyword "exercise" *> (Exercise <$> parseLot)

parsePosition :: Parser Position
parsePosition =
  Position
    <$> L.decimal <* whiteSpace
    <*> parseLot
    <*> parseDisposition
    <*> parseAmount
  where
    parseDisposition :: Parser Disposition
    parseDisposition =
      Long <$ keyword "long"
        <|> Short <$ keyword "short"

parseClosing :: Parser Closing
parseClosing =
  Closing
    <$> ( char '('
            *> whiteSpace
            *> parsePosition
            <* whiteSpace
            <* char ')'
            <* whiteSpace
        )
    <*> parseLot

parseEvent :: Parser Event
parseEvent =
  keyword "open" *> (Open <$> parsePosition)
    <|> keyword "close" *> (Close <$> parseClosing)
    <|> keyword "wash"
      *> keyword "past"
      *> (Wash Past <$> parseTime <*> parseLot)
    <|> keyword "wash"
      *> keyword "present"
      *> (Wash Present <$> parseTime <*> parseLot)
    <|> keyword "wash"
      *> keyword "future"
      *> (Wash Future <$> parseTime <*> parseLot)
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
    <|> keyword "washed" *> (Washed <$> parseAmount)
    <|> keyword "wash" *> parseWash
    <|> keyword "apply"
      *> (WashApply . TL.toStrict <$> parseSymbol <*> parseAmount)
    <|> (keyword "exempt" $> Exempt)
    <|> keyword "account" *> (Account <$> parseText)
    <|> keyword "id" *> (Ident <$> L.decimal)
    <|> keyword "order" *> (Order <$> parseText)
    <|> keyword "strategy" *> (Strategy <$> parseText)
    <|> keyword "note" *> (Note <$> quotedString)
    <|> keyword "meta" *> (Meta <$> parseText <*> parseText)
  where
    parseWash = do
      mres <- optional $ do
        q <- parseAmount
        _ <- char '@' <* whiteSpace
        p <- parseAmount
        pure (q, p)
      _ <- keyword "to"
      sym <- parseSymbol
      pure $ WashTo (TL.toStrict sym) mres

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
