{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Journal.Parse where

import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
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

parse :: Parser (Action Lot)
parse = do
  _time <- parseTime'
  action <-
    asum
      ( map
          keyword
          [ "buy",
            "sell",
            "wash",
            "deposit",
            "withdraw",
            "assign",
            "expire",
            "dividend"
          ]
      )
  _amount <- parseAmount
  _symbol <- parseSymbol
  _price <- single '$' *> parseAmount
  let _details = []
  pure $ case action of
    "buy" -> BuySell Lot {..}
    "sell" -> BuySell Lot {..}
    "wash" -> BuySell Lot {..}
    _ -> error $ "Unexpected action: " ++ T.unpack action

parseTime' :: Parser UTCTime
parseTime' = undefined

parseAmount :: Parser (Amount 6)
parseAmount = undefined

parseSymbol :: Parser Text
parseSymbol = undefined
