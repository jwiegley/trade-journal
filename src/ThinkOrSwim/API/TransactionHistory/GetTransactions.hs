{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module ThinkOrSwim.API.TransactionHistory.GetTransactions where

import Data.Aeson
import Data.Int
import Data.Text as T
import Data.Time

data FixedIncome = FixedIncome
    { bondInterestRate :: Double
    , bondMaturityDate :: UTCTime
    }
    deriving (Eq, Show)

data Option = Option
    { description      :: Maybe Text
    , putCall          :: Maybe PutCall
    , strikePrice      :: Maybe Double
    , expirationDate   :: UTCTime
    , underlyingSymbol :: Maybe Text
    }
    deriving (Eq, Show)

data CashEquivalent = CashEquivalent
    { cashType :: Text
    }
    deriving (Eq, Show)

data AssetType
    = Equity
    | MutualFund
    | OptionAsset Option
    | FixedIncomeAsset FixedIncome
    | CashEquivalentAsset CashEquivalent
    deriving (Eq, Show)

data PutCall
    = Put
    | Call
    deriving (Eq, Show, Enum, Ord)

instance FromJSON PutCall where
  parseJSON = withText "putCall" $ \text ->
    case text of
      "PUT"  -> return Put
      "CALL" -> return Call
      _      -> fail $ "putCall unexpected: " ++ T.unpack text

data Instrument = Instrument
    { assetType :: AssetType
    , symbol    :: Maybe Text
    , cusip     :: Text
    }
    deriving (Eq, Show)

instance FromJSON Instrument where
  parseJSON = withObject "instrument" $ \obj -> do
    assetTypeText        <- obj .:  "assetType"
    cusip                <- obj .:  "cusip"
    symbol               <- obj .:? "symbol"

    -- bondInterestRate     <- obj .:? "bondInterestRate"
    -- bondMaturityDate     <- obj .:? "bondMaturityDate"
    -- description          <- obj .:? "description"
    -- putCall              <- obj .:? "putCall"
    -- optionStrikePrice    <- obj .:? "optionStrikePrice"
    -- optionExpirationDate <- obj .:? "optionExpirationDate"
    -- underlyingSymbol     <- obj .:? "underlyingSymbol"

    assetType <- case assetTypeText of
      "EQUITY"      -> return Equity
      "MUTUAL_FUND" -> return MutualFund
      "OPTION" ->
          fmap OptionAsset $ Option
              <$> obj .:? "description"
              <*> obj .:? "putCall"
              <*> obj .:? "optionStrikePrice"
              <*> obj .:  "optionExpirationDate"
              <*> obj .:? "underlyingSymbol"
      "FIXED_INCOME"    ->
          fmap FixedIncomeAsset $ FixedIncome
              <$> obj .:  "bondInterestRate"
              <*> obj .:  "bondMaturityDate"
      "CASH_EQUIVALENT" ->
          CashEquivalentAsset . CashEquivalent
              <$> obj .:  "type"
      _                 -> fail $ "assetType unexpected: " ++ T.unpack assetTypeText

    return Instrument{..}

data PositionEffect
    = Opening
    | Closing
    | Automatic
    deriving (Eq, Show, Enum, Ord)

instance FromJSON PositionEffect where
  parseJSON = withText "positionEffect" $ \text ->
    case text of
      "OPENING"   -> return Opening
      "CLOSING"   -> return Closing
      "AUTOMATIC" -> return Automatic
      _           -> fail $ "positionEffect unexpected: " ++ T.unpack text

data Instruction
    = Buy
    | Sell
    deriving (Eq, Show, Enum, Ord)

instance FromJSON Instruction where
  parseJSON = withText "instruction" $ \text ->
    case text of
      "BUY"  -> return Buy
      "SELL" -> return Sell
      _      -> fail $ "instruction unexpected: " ++ T.unpack text

data TransactionItem = TransactionItem
    { instrument           :: Maybe Instrument
    , positionEffect       :: Maybe PositionEffect
    , instruction          :: Maybe Instruction
    , parentChildIndicator :: Maybe Text
    , parentOrderKey       :: Maybe Int32
    , cost                 :: Double
    , price                :: Maybe Double
    , amount               :: Maybe Double
    , accountId            :: Int32
    }
    deriving (Eq, Show)

instance FromJSON TransactionItem where
  parseJSON = withObject "transactionItem" $ \obj -> do
    instrument           <- obj .:? "instrument"
    positionEffect       <- obj .:? "positionEffect"
    instruction          <- obj .:? "instruction"
    parentChildIndicator <- obj .:? "parentChildIndicator"
    parentOrderKey       <- obj .:? "parentOrderKey"
    cost                 <- obj .:  "cost"
    price                <- obj .:? "price"
    amount               <- obj .:? "amount"
    accountId            <- obj .:  "accountId"
    return TransactionItem{..}

data Fees = Fees
    { rFee          :: Double
    , additionalFee :: Double
    , cdscFee       :: Double
    , regFee        :: Double
    , otherCharges  :: Double
    , commission    :: Double
    , optRegFee     :: Double
    , secFee        :: Double
    }
    deriving (Eq, Show)

instance FromJSON Fees where
  parseJSON = withObject "fees" $ \obj -> do
    rFee          <- obj .: "rFee"
    additionalFee <- obj .: "additionalFee"
    cdscFee       <- obj .: "cdscFee"
    regFee        <- obj .: "regFee"
    otherCharges  <- obj .: "otherCharges"
    commission    <- obj .: "commission"
    optRegFee     <- obj .: "optRegFee"
    secFee        <- obj .: "secFee"
    return Fees{..}

data AchStatus
    = Approved
    | Rejected
    | Cancel
    | Error_
    deriving (Eq, Show, Enum, Ord)

instance FromJSON AchStatus where
  parseJSON = withText "achStatus" $ \text ->
    case text of
      "Approved" -> return Approved
      "Rejected" -> return Rejected
      "Cancel"   -> return Cancel
      "Error"    -> return Error_
      _          -> fail $ "achStatus unexpected: " ++ T.unpack text

data TransactionType
    = Trade
    | ReceiveAndDeliver
    | DividendOrInterest
    | AchReceipt
    | AchDisbursement
    | CashReceipt
    | CashDisbursement
    | ElectronicFund
    | WireOut
    | WireIn
    | Journal
    | Memorandum
    | MarginCall
    | MoneyMarket
    | SmaAdjustment
    deriving (Eq, Show, Enum, Ord)

instance FromJSON TransactionType where
  parseJSON = withText "transactionType" $ \text ->
    case text of
      "TRADE"                -> return Trade
      "RECEIVE_AND_DELIVER"  -> return ReceiveAndDeliver
      "DIVIDEND_OR_INTEREST" -> return DividendOrInterest
      "ACH_RECEIPT"          -> return AchReceipt
      "ACH_DISBURSEMENT"     -> return AchDisbursement
      "CASH_RECEIPT"         -> return CashReceipt
      "CASH_DISBURSEMENT"    -> return CashDisbursement
      "ELECTRONIC_FUND"      -> return ElectronicFund
      "WIRE_OUT"             -> return WireOut
      "WIRE_IN"              -> return WireIn
      "JOURNAL"              -> return Journal
      "MEMORANDUM"           -> return Memorandum
      "MARGIN_CALL"          -> return MarginCall
      "MONEY_MARKET"         -> return MoneyMarket
      "SMA_ADJUSTMENT"       -> return SmaAdjustment
      _                      -> fail $ "transactionType unexpected: " ++ T.unpack text

data Transaction = Transaction
    { transactionItem               :: TransactionItem
    , fees                          :: Fees
    , accruedInterest               :: Maybe Double
    , achStatus                     :: Maybe AchStatus
    , transactionDescription        :: Text
    , cashBalanceEffectFlag         :: Bool
    , transactionId                 :: Int64
    , transactionSubType            :: Text
    , orderDate                     :: Maybe UTCTime
    , transactionDate               :: UTCTime
    , netAmount                     :: Double
    , dayTradeBuyingPowerEffect     :: Maybe Double
    , requirementReallocationAmount :: Maybe Double
    , sma                           :: Maybe Double
    , orderId                       :: Maybe Text
    , settlementDate                :: UTCTime
    , subAccount                    :: Text
    , clearingReferenceNumber       :: Maybe Text
    , type_                         :: TransactionType
    }
    deriving (Eq, Show)

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \obj -> do
    transactionItem               <- obj .:  "transactionItem"
    fees                          <- obj .:  "fees"
    accruedInterest               <- obj .:? "accruedInterest"
    achStatus                     <- obj .:? "achStatus"
    transactionDescription        <- obj .:  "description"
    cashBalanceEffectFlag         <- obj .:? "cashBalanceEffectFlag" .!= False
    transactionId                 <- obj .:  "transactionId"
    transactionSubType            <- obj .:  "transactionSubType"
    orderDate                     <- obj .:? "orderDate"
    transactionDate               <- obj .:  "transactionDate"
    netAmount                     <- obj .:  "netAmount"
    dayTradeBuyingPowerEffect     <- obj .:? "dayTradeBuyingPowerEffect"
    requirementReallocationAmount <- obj .:? "requirementReallocationAmount"
    sma                           <- obj .:? "sma"
    orderId                       <- obj .:? "orderId"
    settlementDateText            <- obj .:  "settlementDate"
    let settlementDate = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" settlementDateText
    subAccount                    <- obj .:  "subAccount"
    clearingReferenceNumber       <- obj .:? "clearingReferenceNumber"
    type_                         <- obj .:  "type"
    return Transaction{..}
