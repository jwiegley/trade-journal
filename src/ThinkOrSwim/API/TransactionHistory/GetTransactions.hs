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

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Aeson hiding ((.=))
import           Data.Int
import           Data.List (sortBy)
import           Data.Map (Map)
import           Data.Ord
-- import qualified Data.Map as M
import           Data.Semigroup hiding (Option, option)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time

data FixedIncome = FixedIncome
    { _bondInterestRate :: Double
    , _bondMaturityDate :: UTCTime
    }
    deriving (Eq, Ord, Show)

makeClassy ''FixedIncome

data PutCall
    = Put
    | Call
    deriving (Eq, Ord, Show, Enum)

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
    deriving (Eq, Ord, Show)

makeClassy ''Option

data CashEquivalent = CashEquivalent
    { _cashType :: Text
    }
    deriving (Eq, Ord, Show)

makeClassy ''CashEquivalent

data AssetType
    = Equity
    | MutualFund
    | OptionAsset Option
    | FixedIncomeAsset FixedIncome
    | CashEquivalentAsset CashEquivalent
    deriving (Eq, Ord, Show)

makePrisms ''AssetType

data Instrument = Instrument
    { _assetType :: AssetType
    , _symbol    :: Text
    , _cusip     :: Text
    }
    deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show, Enum)

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
    deriving (Eq, Ord, Show, Enum)

makePrisms ''Instruction

instance FromJSON Instruction where
  parseJSON = withText "instruction" $ \text ->
    case text of
      "BUY"  -> return Buy
      "SELL" -> return Sell
      _      -> fail $ "instruction unexpected: " ++ T.unpack text

type AccountId = Int32

data TransactionItem = TransactionItem
    { _transactionInstrument :: Maybe Instrument
    , _positionEffect        :: Maybe PositionEffect
    , _instruction           :: Maybe Instruction
    -- , _parentChildIndicator  :: Maybe Text
    -- , _parentOrderKey        :: Maybe Int32
    , _cost                  :: Double
    , _price                 :: Maybe Double
    , _amount                :: Maybe Double
    , _accountId             :: AccountId
    }
    deriving (Eq, Ord, Show)

makeClassy ''TransactionItem

instance FromJSON TransactionItem where
  parseJSON = withObject "transactionItem" $ \obj -> do
    _transactionInstrument <- obj .:? "instrument"
    _positionEffect        <- obj .:? "positionEffect"
    _instruction           <- obj .:? "instruction"
    -- _parentChildIndicator  <- obj .:? "parentChildIndicator"
    -- _parentOrderKey        <- obj .:? "parentOrderKey"
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
    deriving (Eq, Ord, Show)

makeClassy ''Fees

instance Semigroup Fees where
    x <> y = Fees
        { _rFee          = x^.rFee          + y^.rFee
        , _additionalFee = x^.additionalFee + y^.additionalFee
        , _cdscFee       = x^.cdscFee       + y^.cdscFee
        , _regFee        = x^.regFee        + y^.regFee
        , _otherCharges  = x^.otherCharges  + y^.otherCharges
        , _commission    = x^.commission    + y^.commission
        , _optRegFee     = x^.optRegFee     + y^.optRegFee
        , _secFee        = x^.secFee        + y^.secFee
        }

instance Monoid Fees where
    mempty = Fees
        { _rFee          = 0.0
        , _additionalFee = 0.0
        , _cdscFee       = 0.0
        , _regFee        = 0.0
        , _otherCharges  = 0.0
        , _commission    = 0.0
        , _optRegFee     = 0.0
        , _secFee        = 0.0
        }
    mappend = (<>)

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
    deriving (Eq, Ord, Show, Enum)

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
    deriving (Eq, Ord, Enum)

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

type TransactionId = Int64
type TransactionSubType = Text

data TransactionInfo = TransactionInfo
    { _transactionId          :: TransactionId
    , _transactionSubType     :: TransactionSubType
    , _transactionDate        :: UTCTime
    , _transactionItem_       :: TransactionItem
    , _transactionDescription :: Text
    }
    deriving (Eq, Ord, Show)

makeClassy ''TransactionInfo

data Transaction = Transaction
    { _transactionInfo_              :: TransactionInfo
    , _fees_                         :: Fees
    , _accruedInterest               :: Maybe Double
    , _achStatus                     :: Maybe AchStatus
    , _cashBalanceEffectFlag         :: Bool
    , _transactionOrderDate          :: Maybe UTCTime
    , _netAmount                     :: Double
    , _dayTradeBuyingPowerEffect     :: Maybe Double
    , _requirementReallocationAmount :: Maybe Double
    , _sma                           :: Maybe Double
    , _transactionOrderId            :: Maybe Text
    , _settlementDate                :: UTCTime
    , _subAccount                    :: Text
    , _clearingReferenceNumber       :: Maybe Text
    , _type_                         :: TransactionType
    }
    deriving (Eq, Ord, Show)

makeClassy ''Transaction

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \obj -> do
    _transactionItem_       <- obj .: "transactionItem"
    _transactionId          <- obj .: "transactionId"
    _transactionSubType     <- obj .: "transactionSubType"
    _transactionDate        <- obj .: "transactionDate"
    _transactionDescription <- obj .: "description"
    let _transactionInfo_ = TransactionInfo {..}

    _accruedInterest               <- obj .:? "accruedInterest"
    _achStatus                     <- obj .:? "achStatus"
    _cashBalanceEffectFlag         <- obj .:? "cashBalanceEffectFlag" .!= False
    _transactionOrderDate          <- obj .:? "orderDate"
    _netAmount                     <- obj .:  "netAmount"
    _fees_                         <- obj .:  "fees"
    _dayTradeBuyingPowerEffect     <- obj .:? "dayTradeBuyingPowerEffect"
    _requirementReallocationAmount <- obj .:? "requirementReallocationAmount"
    _sma                           <- obj .:? "sma"
    _transactionOrderId            <- obj .:? "orderId"
    _settlementDateText            <- obj .:  "settlementDate"
    let _settlementDate =
            parseTimeOrError False defaultTimeLocale "%Y-%m-%d" _settlementDateText
    _subAccount                    <- obj .:  "subAccount"
    _clearingReferenceNumber       <- obj .:? "clearingReferenceNumber"
    _type_                         <- obj .:  "type"
    return Transaction{..}

data Order = Order
    { _transactions     :: [Transaction]
    , _orderId          :: Text
    , _orderType        :: TransactionType
    , _orderDate        :: UTCTime
    , _orderDescription :: Text
    , _orderAccountId   :: AccountId
    }
    deriving (Eq, Ord, Show)

makeClassy ''Order

type CUSIPMap       = Map Text Instrument
type OrderId        = Text
type OrdersMap      = Map OrderId Order
type SettlementList = [(UTCTime, Either Transaction OrderId)]

data TransactionHistory = TransactionHistory
    { _allTransactions :: [Transaction]
    , _cusipMap        :: CUSIPMap
    , _ordersMap       :: OrdersMap
    , _settlementList  :: SettlementList
    }
    deriving (Eq, Ord, Show)

makeClassy ''TransactionHistory

newTransactionHistory :: TransactionHistory
newTransactionHistory = TransactionHistory
    { _allTransactions = []
    , _cusipMap        = mempty
    , _ordersMap       = mempty
    , _settlementList  = []
    }

instance FromJSON TransactionHistory where
  parseJSON v@(Array _) = processTransactions <$> parseJSON v
  parseJSON v = error $ "Unexpected transaction history value: " ++ show v

-- Some transactions refer to an instrument, but only by the CUSIP id. It can
-- happen when an options position is closed by assignment, for example. This
-- cleanup pass restores the missing information for that instrument, and as a
-- conesquence builds a complete mapping of CUSIP -> Instrument for all
-- observed instruments.
--
-- Transformations performed on the data from TD Ameritrade:
--
-- 1. Infer missing data
--    a. symbol information lookup
--
-- 2. Coalesce multiple executions into orders
--
-- 3. Calculate capital gains on closing orders
--    a. P/L agaisnt opening price
--    b. wash sale rule

processTransactions :: [Transaction] -> TransactionHistory
processTransactions xs = (`execState` newTransactionHistory) $ do
    mapM_ go (Prelude.reverse xs)
    allTransactions %= Prelude.reverse
    settlementList %= Prelude.reverse
    ordersMap.traverse.transactions %= orderTransactions
  where
    go :: Transaction -> State TransactionHistory ()
    go t = case t^?instrument_._Just of
        Nothing -> check t
        Just inst -> case inst^.symbol of
            "" -> preuse (cusipMap.ix (inst^.cusip)) >>= \case
                Nothing ->
                    error $ "Unknown CUSIP: " ++ T.unpack (inst^.cusip)
                Just inst' ->
                    check $ t & instrument_ ?~ inst'
            _sym -> do
                cusipMap.at (inst^.cusip) ?= inst
                check t

    check :: Transaction -> State TransactionHistory ()
    check t = case t^?instrument_._Just.symbol of
        Just "" -> error $ "symbol has no name: " ++ show t
        _ -> do
            allTransactions %= (t :)

            let sd = t^.settlementDate
            case orderIdAndDate t of
                Just (oid, date) -> preuse (ordersMap.ix oid) >>= \case
                    Nothing -> do
                        let o = Order
                                { _transactions     = [t]
                                , _orderId          = oid
                                , _orderType        = t^.type_
                                , _orderDate        = date
                                , _orderDescription =
                                  t^.transactionInfo_.transactionDescription
                                , _orderAccountId   = t^.accountId_
                                }
                        ordersMap.at oid ?= o
                        settlementList %= ((sd, Right oid) :)
                    Just _ ->
                        ordersMap.ix oid.transactions <>= [t]
                _ ->
                    settlementList %= ((sd, Left t) :)

orderIdAndDate :: Transaction -> Maybe (OrderId, UTCTime)
orderIdAndDate t = liftA2 (,) (t^?baseOrderId) (t^.transactionOrderDate)

-- A Fold over the individual components of a Text split on a separator.
--
-- splitOn :: Text -> Fold String String
-- splitOn :: Text -> Traversal' String String
--
-- splitOn :: Text -> IndexedFold Int String String
-- splitOn :: Text -> IndexedTraversal' Int String String
--
-- Note: This function type-checks as a Traversal but it doesn't satisfy the
-- laws. It's only valid to use it when you don't insert any separator strings
-- while traversing, and if your original Text contains only isolated split
-- strings.
splitOn :: Applicative f => Text -> IndexedLensLike' Int f Text Text
splitOn s f = fmap (T.intercalate s) . go . T.splitOn s
  where
    go = conjoined traverse (indexing traverse) f

baseOrderId :: Traversal' Transaction Text
baseOrderId = transactionOrderId.traverse.splitOn ".".index 0

instrument_ :: Lens' Transaction (Maybe Instrument)
instrument_ = transactionInfo_.transactionItem_.transactionInstrument

accountId_ :: Lens' Transaction AccountId
accountId_ = transactionInfo_.transactionItem_.accountId

infixr 7 <+>
(<+>) :: (Applicative m, Num a) => m a -> m a -> m a
(<+>) = liftA2 (+)

-- Given an order and all the execution transactions that make it up, find the
-- "aggregate" transactions that make it up. For example, if an order for 200
-- shares is executed as four purchases orders of 50 count each, from the
-- point of view of the order, this is one transaction, even if from the point
-- of view of the broker it is four.
orderTransactions :: [Transaction] -> [Transaction]
orderTransactions =
    contractList mergeTransactions
        . sortBy (comparing (^?instrument_._Just.assetType) <>
                  comparing (^?transactionInfo_.transactionDate))

contractList :: (a -> a -> Maybe a) -> [a] -> [a]
contractList _ [] = []
contractList _ [x] = [x]
contractList f (x:y:xs) = case f x y of
    Nothing -> x : contractList f (y:xs)
    Just z  ->     contractList f (z:xs)

mergeTransactionItems :: TransactionItem -> TransactionItem -> Maybe TransactionItem
mergeTransactionItems x y = do
    guard conditions
    pure TransactionItem {..}
  where
    conditions
        = x^.accountId             == y^.accountId
          -- These three could certainly be different, such as when assigning
          -- an option by closing out the short put and buying the equity, but
          -- in that case we simply don't merge the transactions.
        && x^.transactionInstrument == y^.transactionInstrument
        && x^.instruction           == y^.instruction
        && x^.positionEffect        == y^.positionEffect

    _accountId             = x^.accountId
    _amount                = x^.amount <+> y^.amount
    _cost                  = x^.cost + y^.cost
    _instruction           = x^.instruction
    _positionEffect        = x^.positionEffect
    _price                 = x^.price <+> y^.price
    _transactionInstrument = x^.transactionInstrument

mergeTransactionInfos :: TransactionInfo -> TransactionInfo -> Maybe TransactionInfo
mergeTransactionInfos x y = do
    guard conditions
    _transactionItem_ <- mergeTransactionItems (x^.transactionItem_) (y^.transactionItem_)
    pure TransactionInfo {..}
  where
    conditions
        = x^.transactionId          /=  y^.transactionId
        && x^.transactionDescription == y^.transactionDescription
        && x^.transactionSubType     == y^.transactionSubType

    _transactionDescription = x^.transactionDescription
    _transactionSubType     = x^.transactionSubType

    -- jww (2020-04-05): Here is where we discard information when coalescing
    -- multiple transactions into an order.
    _transactionId          = getMax $ Max (x^.transactionId) <> Max (y^.transactionId)
    _transactionDate        = getMax $ Max (x^.transactionDate) <> Max (y^.transactionDate)

mergeTransactions :: Transaction -> Transaction -> Maybe Transaction
mergeTransactions x y = do
    _orderId <- x^?baseOrderId
    yid      <- y^?baseOrderId
    guard $ _orderId == yid
    guard conditions
    _transactionInfo_ <-
        mergeTransactionInfos (x^.transactionInfo_) (y^.transactionInfo_)
    pure Transaction {..}
  where
    conditions
        = x^.achStatus               == y^.achStatus
        && x^.cashBalanceEffectFlag   == y^.cashBalanceEffectFlag
        && x^.clearingReferenceNumber == y^.clearingReferenceNumber
        && x^.transactionOrderId      == y^.transactionOrderId
        && x^.transactionOrderDate    == y^.transactionOrderDate
        && x^.settlementDate          == y^.settlementDate
        && x^.subAccount              == y^.subAccount
        && x^.type_                   == y^.type_

    _accruedInterest               = x^.accruedInterest <+> y^.accruedInterest
    _achStatus                     = x^.achStatus
    _cashBalanceEffectFlag         = x^.cashBalanceEffectFlag
    _clearingReferenceNumber       = x^.clearingReferenceNumber
    _dayTradeBuyingPowerEffect     = x^.dayTradeBuyingPowerEffect <+> y^.dayTradeBuyingPowerEffect
    _fees_                         = x^.fees_ <> y^.fees_
    _netAmount                     = x^.netAmount + y^.netAmount
    _transactionOrderId            = x^.transactionOrderId
    _transactionOrderDate          = x^.transactionOrderDate
    _requirementReallocationAmount = x^.requirementReallocationAmount <+> y^.requirementReallocationAmount
    _settlementDate                = x^.settlementDate
    _sma                           = x^.sma <+> y^.sma
    _subAccount                    = x^.subAccount
    _type_                         = x^.type_

orderFromTransaction :: Transaction -> Order
orderFromTransaction t = Order {..}
  where
    _transactions     = [t]
    _orderId          = T.pack . show $ t^.transactionInfo_.transactionId
    _orderType        = t^.type_
    _orderDate        = t^.transactionInfo_.transactionDate
    _orderDescription = t^.transactionInfo_.transactionDescription
    _orderAccountId   = t^.accountId_
