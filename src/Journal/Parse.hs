{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Journal.Parse (parseJournal, printJournal) where

import Control.Lens hiding (noneOf)
import Data.Char
import Data.Coerce
import Data.Either
import Data.List (intersperse, sort)
import Data.Maybe (mapMaybe)
import Data.Proxy
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

quotedString :: Parser T.Text
quotedString = identPQuoted >>= return . T.pack
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
  keyword "deposit" *> (Deposit <$> parseAmount <*> quotedString)
    <|> keyword "withdraw" *> (Withdraw <$> parseAmount <*> quotedString)
    <|> keyword "buy" *> (Buy <$> parseLot)
    <|> keyword "sell" *> (Sell <$> parseLot)
    <|> keyword "in" *> (TransferIn <$> parseLot <*> quotedString)
    <|> keyword "out" *> (TransferOut <$> parseLot <*> quotedString)
    <|> keyword "wash" *> (Wash <$> parseLot)
    <|> keyword "assign" *> (Assign <$> parseLot)
    <|> keyword "exercise" *> (Assign <$> parseLot)
    <|> keyword "expire" *> (Expire <$> parseLot)
    <|> keyword "dividend" *> (Dividend <$> parseAmount <*> parseLot)
    <|> keyword "interest" *> (Interest <$> parseAmount <*> quotedString)
    <|> keyword "income" *> (Income <$> parseAmount <*> quotedString)
    <|> keyword "credit" *> (Credit <$> parseAmount <*> quotedString)

parseLot :: Parser Lot
parseLot = do
  _amount <- parseAmount
  _symbol <- TL.toStrict <$> parseSymbol
  _price <- parseAmount
  _details <- many parseAnnotation
  let _computed = []
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
    parseWash = do
      mres <- optional $ do
        q <- parseAmount
        _ <- char '@' <* whiteSpace
        p <- parseAmount
        pure (q, p)
      _ <- keyword "to"
      sym <- parseSymbol
      pure $ WashTo (TL.toStrict sym) mres
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
  TL.concat $
    intersperse " " $
      [ printTime (t ^. time),
        printItem (t ^. item)
      ]

printString :: T.Text -> Text
printString = TL.pack . show . TL.fromStrict

printAction :: Action -> Text
printAction = \case
  Deposit amt desc -> "deposit " <> printAmount amt <> " " <> printString desc
  Withdraw amt desc -> "withdraw " <> printAmount amt <> " " <> printString desc
  Buy lot -> "buy " <> printLot lot
  Sell lot -> "sell " <> printLot lot
  TransferIn lot desc -> "in " <> printLot lot <> " " <> TL.fromStrict desc
  TransferOut lot desc -> "out " <> printLot lot <> " " <> TL.fromStrict desc
  Wash lot -> "wash " <> printLot lot
  Assign lot -> "assign " <> printLot lot
  Exercise lot -> "exercise " <> printLot lot
  Expire lot -> "expire " <> printLot lot
  Dividend amt lot -> "dividend " <> printAmount amt <> " " <> printLot lot
  Interest amt desc -> "interest " <> printAmount amt <> " " <> printString desc
  Income amt desc -> "income " <> printAmount amt <> " " <> printString desc
  Credit amt desc -> "credit " <> printAmount amt <> " " <> printString desc

printLot :: Lot -> Text
printLot lot =
  ( TL.concat $
      intersperse
        " "
        ( [ printAmount @0 (lot ^. amount . coerced),
            TL.fromStrict (lot ^. symbol),
            printAmount @4 (lot ^. price . coerced)
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
    annotations =
      mapMaybe
        printAnnotation
        (sort (lot ^. details ++ lot ^. computed))
    (inlineAnns, separateAnns) = partitionEithers annotations
    totalAmount :: forall n. KnownNat n => Amount n -> Text
    totalAmount x = printAmount @n (totaled @n lot x)
    printAnnotation = \case
      Position eff -> Just $
        Left $ case eff of
          Open -> "open"
          Close -> "close"
      Fees x -> Just $ Left $ "fees " <> totalAmount @2 (coerce x)
      Commission x -> Just $ Left $ "commission " <> totalAmount @2 (coerce x)
      Gain x -> Just $ Right $ "gain " <> totalAmount @6 x
      Loss x -> Just $ Right $ "loss " <> totalAmount @6 x
      Washed x -> Just $ Right $ "washed " <> totalAmount @6 x
      WashTo x (Just (q, p)) ->
        Just $
          Left $
            "wash "
              <> printAmount @0 (coerce q)
              <> " @ "
              <> printAmount @4 (coerce p)
              <> " to "
              <> TL.fromStrict x
      WashTo x Nothing -> Just $ Left $ "wash to " <> TL.fromStrict x
      WashApply x amt ->
        Just $ Left $ "apply " <> TL.fromStrict x <> " " <> printAmount @0 (coerce amt)
      Exempt -> Just $ Left "exempt"
      Net _x -> Nothing
      Balance _x -> Nothing
      -- Net x -> Right $ "net " <> printAmount 2 (x ^. coerced)
      -- Balance x -> Right $ "balance " <> printAmount 2 (x ^. coerced)
      Account x -> Just $ Left $ "account " <> printText x
      Trade x -> Just $ Left $ "trade " <> printText x
      Order x -> Just $ Left $ "order " <> printText x
      Transaction x -> Just $ Left $ "transaction " <> printText x
      Meta k v -> Just $ Right $ "meta " <> printText k <> " " <> printText v
    printText t
      | T.all isAlphaNum t = TL.fromStrict t
      | otherwise = "\"" <> TL.replace "\"" "\\\"" (TL.fromStrict t) <> "\""

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

printTime :: UTCTime -> Text
printTime = TL.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

parseAmount :: KnownNat n => Parser (Amount n)
parseAmount =
  read <$> some (digitChar <|> char '.') <* whiteSpace

printAmount :: forall n. KnownNat n => Amount n -> Text
printAmount = TL.pack . amountToString (fromIntegral (natVal (Proxy :: Proxy n)))

parseSymbol :: Parser Text
parseSymbol =
  TL.pack <$> some (satisfy (\c -> isAlphaNum c || c `elem` ['.', '/']))
    <* whiteSpace
