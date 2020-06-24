{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse where

import Control.Monad
import Data.Functor.Identity
import Data.Text
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

symbol :: Text -> Parser Text
symbol = lexeme . string

parse :: Parser Action
parse = do
  _time <- parseTime'
  action <- symbol "buy" <|> symbol "sell" <|> symbol "wash"
  _amount <- parseAmount
  _symbol <- parseSymbol
  _price <- single '$' *> parseAmount
  let _details = []
  pure $ case action of
    "buy" -> BuySell Lot {..}
    "sell" -> BuySell Lot {..}
    "wash" -> BuySell Lot {..}
    _ -> error $ "Unexpected action: " ++ unpack action

parseTime' :: Parser UTCTime
parseTime' = undefined

parseAmount :: Parser (Amount 6)
parseAmount = undefined

parseSymbol :: Parser Text
parseSymbol = undefined
