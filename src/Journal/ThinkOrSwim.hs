{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Journal.ThinkOrSwim where

import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Foldable
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Void (Void)
import GHC.TypeLits (KnownNat)
import Journal.Amount
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ThinkOrSwim = ThinkOrSwim
  { _account :: Text,
    _xacts :: [Csv.NamedRecord],
    _futures :: [Csv.NamedRecord],
    _forex :: [Csv.NamedRecord],
    _orders :: [Csv.NamedRecord],
    _trades :: [Csv.NamedRecord],
    _equities :: [Csv.NamedRecord],
    _options :: [Csv.NamedRecord],
    _pandl :: [Csv.NamedRecord],
    _byOrderId ::
      Map
        B.ByteString
        ([Csv.NamedRecord], [Csv.NamedRecord], [Csv.NamedRecord])
  }
  deriving (Eq, Show)

makeLenses ''ThinkOrSwim

readSection :: Handle -> IO [Csv.NamedRecord]
readSection h =
  fmap (toList . snd)
    . check
    . Csv.decodeByNameWithP pure Csv.defaultDecodeOptions
    =<< readUntilBlank
  where
    readUntilBlank = do
      line <- BL.fromStrict <$> B.hGetLine h
      if BL.null line
        then pure line
        else do
          rest <- readUntilBlank
          pure $ line <> "\n" <> rest
    check :: MonadFail m => Either String a -> m a
    check (Left err) = fail $ "Error: " ++ err
    check (Right res) = pure res

readCsv :: FilePath -> IO ThinkOrSwim
readCsv path = withFile path ReadMode $ \h -> do
  "\65279Account" : "Statement" : "for" : _account : _ <-
    TL.words . TL.decodeUtf8 <$> readLine h
  "" <- readLine h
  "Cash Balance" <- readLine h
  _xacts <- readSection h
  "Futures Statements" <- readLine h
  _futures <- readSection h
  "Forex Statements" <- readLine h
  _forex <- readSection h
  _ <- readLine h
  "\"Total" : "Cash" : _ <-
    TL.words . TL.decodeUtf8 <$> readLine h
  "" <- readLine h
  "" <- readLine h
  "Account Order History" <- readLine h
  _orders <- readSection h
  "Account Trade History" <- readLine h
  _trades <- readSection h
  "Equities" <- readLine h
  _equities <- readSection h
  "Options" <- readLine h
  _options <- readSection h
  "Profits and Losses" <- readLine h
  _pandl <- readSection h
  let _byOrderId = flip execState mempty $ do
        forM_ _xacts $ \xact -> do
          entry (xact ^?! ix "REF #") . _1 <>= [xact]
        scanOrders _2 _orders
        scanOrders _3 _trades
  pure ThinkOrSwim {..}
  where
    entry oid = at oid . non ([], [], [])
    scanOrders l = flip foldM_ Nothing $ \lx x -> do
      let oid = x ^?! ix "Order ID"
      entry
        ( case lx of
            Nothing -> oid
            Just o
              | B.null oid -> o
              | otherwise -> oid
        )
        . l
        <>= [x]
      pure $
        if null x
          then lx
          else Just oid
    readLine :: Handle -> IO ByteString
    readLine h = do
      line <- BL.fromStrict <$> B.hGetLine h
      -- print line
      pure line

type Symbol = Text

data TOSEntry
  = AchCredit
  | AchDebit
  | AdrFee Symbol
  | Bought TOSDevice TOSTrade
  | CashAltInterest (Amount 2) Symbol
  | CourtesyAdjustment
  | CourtesyCredit
  | ForeignTaxWithheld Symbol
  | FundDisbursement
  | IncomingAccountTransfer
  | InterestAdjustment
  | InterestIncome Symbol
  | MarkToMarket
  | MiscellaneousJournalEntry
  | OffCycleInterest Symbol
  | OrdinaryDividend Symbol
  | QualifiedDividend Symbol
  | Rebate
  | RemoveOptionDueToAssignment (Amount 1) Symbol TOSOption
  | RemoveOptionDueToExpiration (Amount 1) Symbol TOSOption
  | Sold TOSDevice TOSTrade
  | TransferBetweenAccounts
  | TransferFromForexAccount
  | TransferInSecurityOrOption
  | TransferOfCash
  | TransferToForexAccount
  | WireIncoming
  deriving (Eq, Show)

data PutCall = Put | Call
  deriving (Eq, Show)

data TOSDevice = Desktop | IPhone | IPad
  deriving (Eq, Show)

data TOSOption = TOSOption
  { optMultiple :: Amount 0,
    optExpirationDate :: Amount 0,
    optKind :: PutCall
  }
  deriving (Eq, Show)

data TOSTrade = TOSTrade
  { tdQuantity :: Amount 0,
    tdSymbol :: Symbol,
    tdOptDetails :: [TOSOption],
    tdPrice :: Amount 0,
    tdExchange :: Maybe Text
  }
  deriving (Eq, Show)

type Parser = ParsecT Void Text Identity

parseTrade :: Parser TOSTrade
parseTrade = do
  tdQuantity <- pure 0
  tdSymbol <- pure ""
  tdOptDetails <- pure []
  tdPrice <- pure 0
  tdExchange <- optional parseExchange
  pure TOSTrade {..}

parseDevice :: Parser TOSDevice
parseDevice =
  (IPhone <$ symbol "tIP")
    <|> (IPad <$ symbol "tIPAD")
    <|> pure Desktop

parseAmount :: KnownNat n => Parser (Amount n)
parseAmount = undefined

parseSymbol :: Parser Symbol
parseSymbol = TL.pack <$> some (oneOf ['A' .. 'Z'])

parseEntry :: Parser TOSEntry
parseEntry =
  (AchCredit <$ symbol "ACH CREDIT RECEIVED")
    <|> (AchDebit <$ symbol "ACH DEBIT RECEIVED")
    <|> (AdrFee <$> (symbol "ADR FEE~" *> parseSymbol))
    <|> (Bought <$> parseDevice <*> (symbol "BOT" *> parseTrade))
    <|> ( CashAltInterest
            <$> parseAmount
            <*> (symbol "CASH ALTERNATIVES INTEREST" *> parseSymbol)
        )
    <|> ( CourtesyAdjustment
            <$ ( symbol "Couresy Adjustment"
                   <|> symbol "Courtesy Adjustment"
                   <|> symbol "Courtesy adjustment"
                   <|> symbol "Courteys adjustment"
               )
        )
    <|> ( CourtesyCredit
            <$ ( symbol "Courtesy Credit"
                   <|> symbol "courtesy credit"
               )
        )
    <|> (ForeignTaxWithheld <$> (symbol "FOREIGN TAX WITHHELD" *> parseSymbol))
    <|> ( FundDisbursement
            <$ symbol
              "CLIENT REQUESTED ELECTRONIC FUNDING DISBURSEMENT (FUNDS NOW)"
        )
    <|> ( IncomingAccountTransfer
            <$ symbol "CASH MOVEMENT OF INCOMING ACCOUNT TRANSFER"
        )
    <|> ( InterestAdjustment
            <$ symbol "FREE BALANCE INTEREST ADJUSTMENT~NO DESCRIPTION"
        )
    <|> ( InterestIncome
            <$> ( symbol "INTEREST INCOME - SECURITIES~"
                    *> (TL.pack <$> manyTill (satisfy (const True)) eof)
                )
        )
    <|> (MarkToMarket <$ symbol "MARK TO THE MARKET")
    <|> (MiscellaneousJournalEntry <$ symbol "MISCELLANEOUS JOURNAL ENTRY")
    <|> (OffCycleInterest <$> (symbol "OFF-CYCLE INTEREST~" *> parseSymbol))
    <|> (OrdinaryDividend <$> (symbol "ORDINARY DIVIDEND~" *> parseSymbol))
    <|> (QualifiedDividend <$> (symbol "QUALIFIED DIVIDEND~" *> parseSymbol))
    <|> (Rebate <$ symbol "REBATE")
    <|> ( symbol "REMOVAL OF OPTION DUE TO ASSIGNMENT"
            *> ( RemoveOptionDueToAssignment
                   <$> parseAmount
                   <*> parseSymbol
                   <*> parseOption
               )
        )
    <|> ( symbol "REMOVAL OF OPTION DUE TO EXPIRATION"
            *> ( RemoveOptionDueToExpiration
                   <$> parseAmount
                     <*> parseSymbol
                     <*> parseOption
               )
        )
    <|> (Sold <$> parseDevice <*> (symbol "SOLD" *> parseTrade))
    <|> ( TransferBetweenAccounts
            <$ symbol "INTERNAL TRANSFER BETWEEN ACCOUNTS OR ACCOUNT TYPES"
        )
    <|> (TransferFromForexAccount <$ symbol "TRANSFER FROM FOREX ACCOUNT")
    <|> ( TransferInSecurityOrOption
            <$ symbol "TRANSFER OF SECURITY OR OPTION IN"
        )
    <|> (TransferOfCash <$ symbol "INTERNAL TRANSFER OF CASH")
    <|> (TransferToForexAccount <$ symbol "TRANSFER TO FOREX ACCOUNT")
    <|> (WireIncoming <$ symbol "WIRE INCOMING")

parseOption :: Parser TOSOption
parseOption = undefined

parseExchange :: Parser Text
parseExchange =
  asum $
    map
      symbol
      [ "AMEX",
        "ARCA",
        "BATS",
        "BOX",
        "C2",
        "CBOE",
        "EDGX",
        "ISE",
        "ISE GEMINI",
        "MIAX",
        "NASDAQ",
        "NASDAQ OM",
        "NYSE",
        "PHLX"
      ]

whiteSpace :: Parser ()
whiteSpace = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "#"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

symbol :: Text -> Parser Text
symbol = lexeme . string

testParser :: Text -> Either (ParseErrorBundle Text Void) TOSEntry
testParser = parse parseEntry ""
