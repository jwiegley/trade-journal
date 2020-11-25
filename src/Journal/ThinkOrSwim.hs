{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Journal.ThinkOrSwim where

import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char
import qualified Data.Csv as Csv
import Data.Foldable
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time
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
  deriving (Eq)

instance Show PutCall where
  show Put = "PUT"
  show Call = "CALL"

data TOSDevice = Desktop | IPhone | IPad | Keys [Text] Text
  deriving (Eq, Show)

data OptionExpirationDate = OptionExpirationDate
  { expirationDay :: Day,
    expirationName :: Text
  }
  deriving (Eq, Show)

data TOSOption = TOSOption
  { opMult :: Int,
    opEx :: OptionExpirationDate,
    opStrike :: Amount 2,
    opKind :: PutCall
  }
  deriving (Eq)

instance Show TOSOption where
  show TOSOption {..} =
    show opMult
      ++ " "
      ++ show opEx
      ++ " "
      ++ show opStrike
      ++ " "
      ++ show opKind

data FutureOptionExpirationDate = FutureOptionExpirationDate
  { futExpirationMon :: Int,
    futExpirationYear :: Int,
    futExpirationName :: Text
  }
  deriving (Eq, Show)

data TOSFuturesOption = TOSFuturesOption
  { futOpMultNum :: Int,
    futOpMultDen :: Int,
    futOpEx :: FutureOptionExpirationDate,
    futOpContract :: Symbol,
    futOpStrike :: Amount 2,
    futOpKind :: PutCall
  }
  deriving (Eq, Show)

data TOSOptionTrade
  = SingleOption TOSOption
  | OptionStrategy Text [Either Symbol TOSOption]
  | SingleFuturesOption TOSFuturesOption
  | FuturesOptionStrategy Text [Either Symbol TOSFuturesOption]
  deriving (Eq)

instance Show TOSOptionTrade where
  show (SingleOption opt) = show opt
  show (OptionStrategy _ opts) = show opts
  show (SingleFuturesOption fut) = show fut
  show (FuturesOptionStrategy _ futs) = show futs

data TOSTrade = TOSTrade
  { tdQuantity :: Amount 0,
    tdSymbol :: Symbol,
    tdOptDetails :: Maybe TOSOptionTrade,
    tdPrice :: Amount 2,
    tdExchange :: Maybe Text
  }
  deriving (Eq)

instance Show TOSTrade where
  show TOSTrade {..} =
    show tdQuantity
      ++ case tdOptDetails of
        Just (OptionStrategy strat _) -> " " ++ TL.unpack strat
        Just (FuturesOptionStrategy strat _) -> " " ++ TL.unpack strat
        _ -> ""
      ++ " "
      ++ TL.unpack tdSymbol
      ++ " "
      ++ show tdOptDetails
      ++ " @"
      ++ show tdPrice
      ++ maybe "" (\x -> " " ++ TL.unpack x) tdExchange

type Parser = ParsecT Void Text Identity

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
  (IPhone <$ symbol "tIP")
    <|> (IPad <$ symbol "tIPAD")
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
  str <- some $ satisfy $ \c -> isDigit c || c `elem` ['.', '-', '+', ',']
  whiteSpace
  case ( if head str == '.'
           then '0' : str
           else str
       )
    ^? _Amount of
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
  (AchCredit <$ symbol "ACH CREDIT RECEIVED")
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

test :: FilePath -> IO ()
test path = do
  contents <- TL.readFile path
  forM_ (TL.lines contents) $ parseTest parseEntry
