{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkOrSwim.API.TransactionHistory.GetTransactions where

import           Control.Lens
import           Control.Monad.State
import           Data.Aeson hiding ((.=))
import           Data.Int
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text as T
import           Data.Time

data FixedIncome = FixedIncome
    { _bondInterestRate :: Double
    , _bondMaturityDate :: UTCTime
    }
    deriving (Eq, Show)

makeClassy ''FixedIncome

data PutCall
    = Put
    | Call
    deriving (Eq, Show, Enum, Ord)

makePrisms ''PutCall

instance FromJSON PutCall where
  parseJSON = withText "putCall" $ \text ->
    case text of
      "PUT"  -> return Put
      "CALL" -> return Call
      _      -> fail $ "putCall unexpected: " ++ T.unpack text

data Option = Option
    { _description      :: Text
    , _putCall          :: PutCall
    , _strikePrice      :: Double
    , _expirationDate   :: UTCTime
    , _underlyingSymbol :: Text
    }
    deriving (Eq, Show)

makeClassy ''Option

data CashEquivalent = CashEquivalent
    { _cashType :: Text
    }
    deriving (Eq, Show)

makeClassy ''CashEquivalent

data AssetType
    = Equity
    | MutualFund
    | OptionAsset Option
    | FixedIncomeAsset FixedIncome
    | CashEquivalentAsset CashEquivalent
    deriving (Eq, Show)

makePrisms ''AssetType

data Instrument = Instrument
    { _assetType :: AssetType
    , _symbol    :: Text
    , _cusip     :: Text
    }
    deriving (Eq, Show)

makeClassy ''Instrument

instance FromJSON Instrument where
  parseJSON = withObject "instrument" $ \obj -> do
    _assetTypeText <- obj .:  "assetType"
    _cusip         <- obj .:  "cusip"
    _symbol        <- obj .:? "symbol" .!= ""

    -- _bondInterestRate     <- obj .:? "bondInterestRate"
    -- _bondMaturityDate     <- obj .:? "bondMaturityDate"
    -- _description          <- obj .:? "description"
    -- _putCall              <- obj .:? "putCall"
    -- _optionStrikePrice    <- obj .:? "optionStrikePrice"
    -- _optionExpirationDate <- obj .:? "optionExpirationDate"
    -- _underlyingSymbol     <- obj .:? "underlyingSymbol"

    _assetType <- case _assetTypeText of
      "EQUITY"      -> return Equity
      "MUTUAL_FUND" -> return MutualFund
      "OPTION" ->
          fmap OptionAsset $ Option
              <$> obj .:? "description"       .!= ""
              <*> obj .:? "putCall"           .!= Call
              <*> obj .:? "optionStrikePrice" .!= 0.0
              <*> obj .:  "optionExpirationDate"
              <*> obj .:? "underlyingSymbol"  .!= ""
      "FIXED_INCOME"    ->
          fmap FixedIncomeAsset $ FixedIncome
              <$> obj .: "bondInterestRate"
              <*> obj .: "bondMaturityDate"
      "CASH_EQUIVALENT" ->
          CashEquivalentAsset . CashEquivalent
              <$> obj .: "type"

      _ -> fail $ "assetType unexpected: " ++ T.unpack _assetTypeText

    return Instrument{..}

data PositionEffect
    = Opening
    | Closing
    | Automatic
    deriving (Eq, Show, Enum, Ord)

makePrisms ''PositionEffect

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

makePrisms ''Instruction

instance FromJSON Instruction where
  parseJSON = withText "instruction" $ \text ->
    case text of
      "BUY"  -> return Buy
      "SELL" -> return Sell
      _      -> fail $ "instruction unexpected: " ++ T.unpack text

data TransactionItem = TransactionItem
    { _transactionInstrument :: Maybe Instrument
    , _positionEffect        :: Maybe PositionEffect
    , _instruction           :: Maybe Instruction
    , _parentChildIndicator  :: Maybe Text
    , _parentOrderKey        :: Maybe Int32
    , _cost                  :: Double
    , _price                 :: Maybe Double
    , _amount                :: Maybe Double
    , _accountId             :: Int32
    }
    deriving (Eq, Show)

makeClassy ''TransactionItem

instance FromJSON TransactionItem where
  parseJSON = withObject "transactionItem" $ \obj -> do
    _transactionInstrument <- obj .:? "instrument"
    _positionEffect        <- obj .:? "positionEffect"
    _instruction           <- obj .:? "instruction"
    _parentChildIndicator  <- obj .:? "parentChildIndicator"
    _parentOrderKey        <- obj .:? "parentOrderKey"
    _cost                  <- obj .:  "cost"
    _price                 <- obj .:? "price"
    _amount                <- obj .:? "amount"
    _accountId             <- obj .:  "accountId"
    return TransactionItem{..}

data Fees = Fees
    { _rFee          :: Double
    , _additionalFee :: Double
    , _cdscFee       :: Double
    , _regFee        :: Double
    , _otherCharges  :: Double
    , _commission    :: Double
    , _optRegFee     :: Double
    , _secFee        :: Double
    }
    deriving (Eq, Show)

makeClassy ''Fees

instance FromJSON Fees where
  parseJSON = withObject "fees" $ \obj -> do
    _rFee          <- obj .: "rFee"
    _additionalFee <- obj .: "additionalFee"
    _cdscFee       <- obj .: "cdscFee"
    _regFee        <- obj .: "regFee"
    _otherCharges  <- obj .: "otherCharges"
    _commission    <- obj .: "commission"
    _optRegFee     <- obj .: "optRegFee"
    _secFee        <- obj .: "secFee"
    return Fees{..}

data AchStatus
    = Approved
    | Rejected
    | Cancel
    | Error_
    deriving (Eq, Show, Enum, Ord)

makePrisms ''AchStatus

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
    deriving (Eq, Enum, Ord)

makePrisms ''TransactionType

instance Show TransactionType where
    show = \case
      Trade              -> "TRADE"
      ReceiveAndDeliver  -> "RECEIVE_AND_DELIVER"
      DividendOrInterest -> "DIVIDEND_OR_INTEREST"
      AchReceipt         -> "ACH_RECEIPT"
      AchDisbursement    -> "ACH_DISBURSEMENT"
      CashReceipt        -> "CASH_RECEIPT"
      CashDisbursement   -> "CASH_DISBURSEMENT"
      ElectronicFund     -> "ELECTRONIC_FUND"
      WireOut            -> "WIRE_OUT"
      WireIn             -> "WIRE_IN"
      Journal            -> "JOURNAL"
      Memorandum         -> "MEMORANDUM"
      MarginCall         -> "MARGIN_CALL"
      MoneyMarket        -> "MONEY_MARKET"
      SmaAdjustment      -> "SMA_ADJUSTMENT"

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
    { _transactionItem_              :: TransactionItem
    , _fees_                         :: Fees
    , _accruedInterest               :: Maybe Double
    , _achStatus                     :: Maybe AchStatus
    , _transactionDescription        :: Text
    , _cashBalanceEffectFlag         :: Bool
    , _transactionId                 :: Int64
    , _transactionSubType            :: Text
    , _orderDate                     :: Maybe UTCTime
    , _transactionDate               :: UTCTime
    , _netAmount                     :: Double
    , _dayTradeBuyingPowerEffect     :: Maybe Double
    , _requirementReallocationAmount :: Maybe Double
    , _sma                           :: Maybe Double
    , _orderId                       :: Maybe Text
    , _settlementDate                :: UTCTime
    , _subAccount                    :: Text
    , _clearingReferenceNumber       :: Maybe Text
    , _type_                         :: TransactionType
    }
    deriving (Eq, Show)

makeClassy ''Transaction

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \obj -> do
    _transactionItem_              <- obj .:  "transactionItem"
    _fees_                         <- obj .:  "fees"
    _accruedInterest               <- obj .:? "accruedInterest"
    _achStatus                     <- obj .:? "achStatus"
    _transactionDescription        <- obj .:  "description"
    _cashBalanceEffectFlag         <- obj .:? "cashBalanceEffectFlag" .!= False
    _transactionId                 <- obj .:  "transactionId"
    _transactionSubType            <- obj .:  "transactionSubType"
    _orderDate                     <- obj .:? "orderDate"
    _transactionDate               <- obj .:  "transactionDate"
    _netAmount                     <- obj .:  "netAmount"
    _dayTradeBuyingPowerEffect     <- obj .:? "dayTradeBuyingPowerEffect"
    _requirementReallocationAmount <- obj .:? "requirementReallocationAmount"
    _sma                           <- obj .:? "sma"
    _orderId                       <- obj .:? "orderId"
    _settlementDateText            <- obj .:  "settlementDate"
    let _settlementDate =
            parseTimeOrError False defaultTimeLocale "%Y-%m-%d" _settlementDateText
    _subAccount                    <- obj .:  "subAccount"
    _clearingReferenceNumber       <- obj .:? "clearingReferenceNumber"
    _type_                         <- obj .:  "type"
    return Transaction{..}

type CUSIPMap = Map Text Instrument

data TransactionHistory = TransactionHistory
    { getTransactionHistory :: [Transaction]
    , cusipMap              :: CUSIPMap
    }
    deriving (Eq, Show)

instance FromJSON TransactionHistory where
  parseJSON v@(Array _) = do
      (getTransactionHistory, cusipMap) <- cleanupTransactions <$> parseJSON v
      pure TransactionHistory {..}
  parseJSON v = error $ "Unexpected transaction history value: " ++ show v

cleanupTransactions :: [Transaction] -> ([Transaction], CUSIPMap)
cleanupTransactions xs =
    flip runState (mempty :: CUSIPMap)
        $ mapM (fmap check . go) (Prelude.reverse xs)
  where
    go :: Transaction -> State CUSIPMap Transaction
    go t =
        case t^.transactionItem_.transactionInstrument of
            Nothing -> pure t
            Just i -> case i^.symbol of
                ""  -> do
                    i' <- preuse (ix (i^.cusip))
                    pure $ t & transactionItem_.transactionInstrument .~ i'
                _sym -> do
                    at (i^.cusip) ?= i
                    pure t

    check :: Transaction -> Transaction
    check t =
        case t^?transactionItem_.transactionInstrument._Just.symbol of
            Just "" -> error $ "symbol has no name: " ++ show t
            _ -> t

newtype OrderHistory = OrderHistory { getOrderHistory :: [Transaction] }

determineOrders :: TransactionHistory -> OrderHistory
determineOrders (TransactionHistory xs _) =
    let (ts, m) = runState (mapM go xs) (mempty :: Map Text [Transaction])
    in OrderHistory $ mconcat $ M.elems m <> ts
  where
    go t = case T.splitOn "." <$> (t^.orderId) of
        Nothing -> pure [t]
        Just (oid:_) -> do
            mres <- preuse (ix oid)
            case mres of
                Nothing -> at oid ?= [t]
                Just _  -> ix oid <>= [t]
            pure []

{- Transformations on the data:

- Read transactions from TD Ameritrade
- Infer any missing data
- Coalesce executions into orders
- Calculate effect of capital gains, include the wash sale rule

 -}
