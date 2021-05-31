{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Journal.ThinkOrSwim.Parser where

import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import qualified Data.Csv as Csv
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time
import Data.Void (Void)
import Debug.Trace
import GHC.TypeLits (KnownNat)
import Journal.Amount
import Journal.ThinkOrSwim.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = ParsecT Void Text Identity

parseSection :: Csv.FromNamedRecord a => Parser [a]
parseSection = do
  text <-
    manyTill
      anySingle
      (try (newline <* many (char ' ') <* newline))
  if length text == 0 || length (lines text) < 2
    then [] <$ whiteSpace
    else do
      -- traceM $ "parseSection: " ++ text
      let bs = TL.encodeUtf8 . TL.pack $ text
      case Csv.decodeByName bs of
        Left err -> fail $ "Error: " ++ err
        Right (_, res) -> pure $ toList res

parseMDY :: Parser Day
parseMDY = do
  mon <- L.decimal
  _ <- char '/'
  day <- L.decimal
  _ <- char '/'
  year <- L.decimal
  whiteSpace
  pure $ fromGregorian (2000 + year) mon day

parseDollars :: Parser (Amount 2)
parseDollars = char '$' *> parseAmount

maybeQuoted :: Parser a -> Parser a
maybeQuoted parser = do
  _ <- optional $ char '"' <* whiteSpace
  res <- parser
  _ <- optional $ char '"' <* whiteSpace
  pure res

parseCsv :: Parser ThinkOrSwim
parseCsv = do
  traceM "parseCsv..1"
  _ <- optional (char '\65279')

  traceM "parseCsv..2"
  symbol_ "Account Statement for"
  _account <- TL.pack . (show :: Integer -> String) <$> L.decimal <* whiteSpace
  _name <- symbol "(" <* someTill (alphaNumChar <* whiteSpace) (symbol ")")
  symbol_ "since"
  _since <- parseMDY
  symbol_ "through"
  _until <- parseMDY

  traceM "parseCsv..3"
  symbol_ "Cash Balance"
  _xacts <- parseSection

  traceM "parseCsv..4"
  symbol_ "Futures Statements"
  traceM "Futures Statements"
  _futures <- parseSection

  traceM "parseCsv..5"
  symbol_ "Forex Statements"
  traceM "Forex Statements"
  _forex <- parseSection

  traceM "parseCsv..6"
  _total <- whiteSpace *> maybeQuoted (symbol_ "Total Cash" *> parseDollars)

  traceM "parseCsv..7"
  symbol_ "Account Order History"
  traceM "Account Order History"
  _orders <- parseSection

  traceM "parseCsv..8"
  symbol_ "Account Trade History"
  traceM "Account Trade History"
  _trades <- parseSection

  traceM "parseCsv..9"
  symbol_ "Equities"
  traceM "Equities"
  _equities <- parseSection

  traceM "parseCsv..10"
  symbol_ "Options"
  traceM "Options"
  _options <- parseSection

  traceM "parseCsv..11"
  _futuresOptions <- fmap (fromMaybe []) $
    optional $ do
      symbol_ "Futures Options"
      traceM "Futures Options"
      parseSection

  traceM "parseCsv..12"
  symbol_ "Profits and Losses"
  traceM "Profits and Losses"
  _profitAndLoss <- parseSection

  traceM "parseCsv..13"
  symbol_ "Forex Account Summary"
  traceM "Forex Account Summary"
  _forexCash <- symbol_ "Forex Cash" *> char ',' *> maybeQuoted parseDollars
  traceM "parseCsv..14"
  _forexUnrealizedPL <-
    symbol_ "Forex Unrealized P/L" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..15"
  _forexFloatingPL <-
    symbol_ "Forex Floating P/L" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..16"
  _forexEquity <- symbol_ "Forex Equity" *> char ',' *> maybeQuoted parseDollars
  traceM "parseCsv..17"
  _forexMargin <- symbol_ "Forex Margin" *> char ',' *> maybeQuoted parseDollars
  traceM "parseCsv..18"
  _forexBuyingPower <-
    symbol_ "Forex Buying Power" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..19"
  _forexRiskLevel <-
    symbol_ "Forex Risk Level" *> char ','
      *> (parseAmount :: Parser (Amount 2)) <* char '%' <* whiteSpace
  traceM "parseCsv..20"
  _forexCommissionsYTD <-
    symbol_ "Forex Commissions YTD" *> char ','
      *> maybeQuoted parseDollars
  traceM $ "parseCsv..20b: " ++ show _forexCommissionsYTD

  let _forexSummary = ForexAccountSummary {..}

  traceM "parseCsv..21"
  symbol_ "Account Summary"
  traceM "Account Summary"
  _netLiquidatingValue <-
    symbol_ "Net Liquidating Value" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..22"
  _stockBuyingPower <-
    symbol_ "Stock Buying Power" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..23"
  _optionBuyingPower <-
    symbol_ "Option Buying Power" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..24"
  _equityCommissionsFeesYTD <-
    symbol_ "Equity Commissions & Fees YTD" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..25"
  _futuresCommissionsFeesYTD <-
    symbol_ "Futures Commissions & Fees YTD" *> char ','
      *> maybeQuoted parseDollars
  traceM "parseCsv..26"
  _totalCommissionsFeesYTD <-
    symbol_ "Total Commissions & Fees YTD" *> char ','
      *> maybeQuoted parseDollars

  let _accountSummary = AccountSummary {..}
  traceM "_accountSummary"

  traceM "parseCsv..27"
  let _byOrderId = flip execState mempty $ do
        forM_ _xacts $ \xact -> do
          entry (xactRefNo xact) . _1 <>= [xact]
        scanOrders _2 _orders
        scanOrders _3 _trades
  traceM "_byOrderId"

  traceM "parseCsv..28"
  eof

  traceM "parseCsv..29"
  pure ThinkOrSwim {..}
  where
    entry oid = at oid . non ([], [], [])
    scanOrders l = flip foldM_ Nothing $ \lx x -> do
      let oid = x ^?! ix "Order ID"
      entry
        (TL.decodeUtf8 (BL.fromStrict ( case lx of
            Nothing -> oid
            Just o
              | B.null oid -> o
              | otherwise -> oid
        )))
        . l
        <>= [x]
      pure $
        if null x
          then lx
          else Just oid

parseStrategy :: Parser Text
parseStrategy =
  symbol "VERTICAL"
    <|> symbol "COMBO"
    <|> symbol "COVERED"
    <|> symbol "DIAGONAL"

parseTrade :: Parser TOSTrade
parseTrade = do
  tdQuantity <- parseAmount
  strategy <- optional parseStrategy
  tdSymbol <- parseSymbol
  tdOptDetails <-
    case strategy of
      Nothing ->
        try (Just . SingleFuturesOption <$> parseFuturesOption)
          <|> Just . SingleOption <$> parseOption
          <|> pure Nothing
      Just strat ->
        try (Just . FuturesOptionStrategy strat <$> parseFuturesOptionStrategy)
          <|> try (Just . OptionStrategy strat <$> parseOptionStrategy)
  tdPrice <- char '@' *> parseAmount
  tdExchange <- optional parseExchange
  pure TOSTrade {..}

matchup :: [a] -> [b] -> [c] -> [(a, b, c)]
matchup xs ys zs =
  let n = lcm (length xs) (lcm (length ys) (length zs))
   in zip3
        (concat (replicate (n `div` length xs) xs))
        (concat (replicate (n `div` length ys) ys))
        (concat (replicate (n `div` length zs) zs))

parseFuturesOptionStrategy :: Parser [Either Symbol TOSFuturesOption]
parseFuturesOptionStrategy = do
  futOpMultNum <- read <$> some (satisfy isDigit)
  _ <- char '/'
  futOpMultDen <- read <$> some (satisfy isDigit)
  whiteSpace
  futOpExs <- parseSeparated (char '/') parseFutOpExDate
  futOpContract <- parseSymbol
  futOpStrikes <- parseSeparated (char '/') (parseAmount @2)
  futOpKinds <-
    parseSeparated
      (char '/')
      (Right <$> parsePutCall <|> Left <$> parseSymbol)
  pure $
    flip map (matchup futOpExs futOpStrikes futOpKinds) $
      \(futOpEx, futOpStrike, e) ->
        case e of
          Left sym -> Left sym
          Right futOpKind -> Right TOSFuturesOption {..}

parseFuturesOption :: Parser TOSFuturesOption
parseFuturesOption = do
  futOpMultNum <- read <$> some (satisfy isDigit)
  _ <- char '/'
  futOpMultDen <- read <$> some (satisfy isDigit)
  whiteSpace
  futOpEx <- parseFutOpExDate
  futOpContract <- parseSymbol
  futOpStrike <- parseAmount @2
  futOpKind <- parsePutCall
  pure TOSFuturesOption {..}

parseFutOpExDate :: Parser FutureOptionExpirationDate
parseFutOpExDate = do
  mon <- parseMonth <* whiteSpace
  year <- some (satisfy isDigit) <* whiteSpace
  postDesc <- many $
    try $ do
      whiteSpace
      _ <- char '('
      desc <- some (satisfy isAlphaNum)
      _ <- char ')'
      pure $ TL.pack $ "(" ++ desc ++ ")"
  whiteSpace
  pure
    FutureOptionExpirationDate
      { futExpirationMon = monToInt mon,
        futExpirationYear = 2000 + read year,
        futExpirationName =
          mon
            <> " "
            <> TL.pack year
            <> TL.concat [" " <> x | x <- postDesc]
      }

parseOptionStrategy :: Parser [Either Symbol TOSOption]
parseOptionStrategy = do
  opMult <- read <$> some (satisfy isDigit)
  whiteSpace
  opExs <- parseSeparated (char '/') parseOpExDate
  opStrikes <- parseSeparated (char '/') (parseAmount @2)
  opKinds <-
    parseSeparated
      (char '/')
      (Right <$> parsePutCall <|> Left <$> parseSymbol)
  pure $
    flip map (matchup opExs opStrikes opKinds) $
      \(opEx, opStrike, e) ->
        case e of
          Left sym -> Left sym
          Right opKind -> Right TOSOption {..}

parseOption :: Parser TOSOption
parseOption = do
  opMult <- read <$> some (satisfy isDigit)
  whiteSpace
  opEx <- parseOpExDate
  opStrike <- parseAmount
  opKind <- parsePutCall
  pure TOSOption {..}

parseOpExDate :: Parser OptionExpirationDate
parseOpExDate = do
  preDesc <- many $ do
    _ <- char '('
    desc <- some (satisfy isAlphaNum)
    _ <- char ')'
    whiteSpace
    pure $ TL.pack $ "(" ++ desc ++ ")"
  day <- many (satisfy isDigit) <* whiteSpace
  mon <- parseMonth <* whiteSpace
  year <- some (satisfy isDigit) <* whiteSpace
  whiteSpace
  pure
    OptionExpirationDate
      { expirationDay =
          fromGregorian (2000 + read year) (monToInt mon) (read day),
        expirationName =
          TL.concat [x <> " " | x <- preDesc]
            <> TL.pack day
            <> " "
            <> mon
            <> " "
            <> TL.pack year
      }

monToInt :: Text -> Int
monToInt "JAN" = 1
monToInt "FEB" = 2
monToInt "MAR" = 3
monToInt "APR" = 4
monToInt "MAY" = 5
monToInt "JUN" = 6
monToInt "JUL" = 7
monToInt "AUG" = 8
monToInt "SEP" = 9
monToInt "OCT" = 10
monToInt "NOV" = 11
monToInt "DEC" = 12
monToInt m = error $ "Unexpected month: " ++ TL.unpack m

parseMonth :: Parser Text
parseMonth =
  symbol "JAN"
    <|> symbol "FEB"
    <|> symbol "MAR"
    <|> symbol "APR"
    <|> symbol "MAY"
    <|> symbol "JUN"
    <|> symbol "JUL"
    <|> symbol "AUG"
    <|> symbol "SEP"
    <|> symbol "OCT"
    <|> symbol "NOV"
    <|> symbol "DEC"

parsePutCall :: Parser PutCall
parsePutCall = Put <$ symbol "PUT" <|> Call <$ symbol "CALL"

parseDevice :: Parser TOSDevice
parseDevice =
  (IPad <$ try (symbol "tIPAD"))
    <|> (IPhone <$ symbol "tIP")
    <|> ( Keys
            <$> ( symbol "KEY:"
                    *> some
                      ( symbol "Ctrl"
                          <|> symbol "Shift"
                          <|> symbol "Alt"
                      )
                )
            <*> ( TL.pack . (: [])
                    <$> oneOf ['A' .. 'Z'] <* space
                )
        )
    <|> pure Desktop

parseAmount :: KnownNat n => Parser (Amount n)
parseAmount = do
  _ <- optional $ char '+'
  str <- some $ satisfy $ \c -> isDigit c || c `elem` ['.', '-', ',']
  whiteSpace
  case ( if head str == '.'
           then '0' : str
           else str
       )
    ^.. folded . filtered (/= ',') ^? _Amount of
    Nothing -> fail $ "Could not parse amount: " ++ str
    Just n -> pure n

parseSymbol :: Parser Symbol
parseSymbol =
  fmap TL.pack (many (satisfy (\c -> isAlphaNum c || c `elem` ['/', ':'])))
    <* whiteSpace

parseSeparated :: Parser b -> Parser a -> Parser [a]
parseSeparated s p = liftA2 (:) p (many (s *> p))

parseEntry :: Parser TOSEntry
parseEntry =
  try (AchCredit <$ symbol "ACH CREDIT RECEIVED")
    <|> (AchDebit <$ symbol "ACH DEBIT RECEIVED")
    <|> (AdrFee <$> (symbol "ADR FEE~" *> parseSymbol))
    <|> try (Bought <$> parseDevice <*> (symbol "BOT" *> parseTrade))
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
    <|> (Total <$ symbol "TOTAL")

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

symbol_ :: Text -> Parser ()
symbol_ = void . lexeme . string

testParser :: Text -> Either (ParseErrorBundle Text Void) TOSEntry
testParser = parse parseEntry ""

test :: FilePath -> IO ()
test path = do
  contents <- TL.readFile path
  forM_ (TL.lines contents) $ parseTest parseEntry

parseFile :: Show a => Parser a -> FilePath -> IO ()
parseFile parser = parseTest parser <=< TL.readFile

readCsv :: FilePath -> IO (Either (ParseErrorBundle Text Void) ThinkOrSwim)
readCsv path = parse parseCsv path <$> TL.readFile path
