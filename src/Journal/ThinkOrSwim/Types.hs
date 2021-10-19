{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.ThinkOrSwim.Types where

import Amount
import Control.Lens
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.Generics
import Text.Read (readMaybe)

data TOSTransaction = TOSTransaction
  { _xactDate :: !Text,
    _xactTime :: !Text,
    _xactType :: !Text,
    _xactRefNo :: !Text,
    _xactDescription :: !Text,
    _xactMiscFees :: !(Amount 2),
    _xactCommissionsAndFees :: !(Amount 2),
    _xactAmount :: !(Amount 2),
    _xactBalance :: !(Amount 2)
  }
  deriving (Generic, Eq, Show)

readAmount :: String -> Amount 2
readAmount "" = 0
readAmount ('(' : xs) = - (readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

instance Csv.FromNamedRecord TOSTransaction where
  parseNamedRecord m =
    TOSTransaction
      <$> m .: "DATE"
      <*> m .: "TIME"
      <*> m .: "TYPE"
      <*> m .: "REF #"
      <*> m .: "DESCRIPTION"
      <*> (readAmount <$> m .: "Misc Fees")
      <*> (readAmount <$> m .: "Commissions & Fees")
      <*> (readAmount <$> m .: "AMOUNT")
      <*> (readAmount <$> m .: "BALANCE")

makeLenses ''TOSTransaction

data TOSFuture = TOSFuture
  { _futureTradeDate :: !Text,
    _futureExecDate :: !Text,
    _futureExecTime :: !Text,
    _futureType :: !Text,
    _futureRefNo :: !Text,
    _futureDescription :: !Text,
    _futureMiscFees :: !Text,
    _futureCommissionsAndFees :: !Text,
    _futureAmount :: !Text,
    _futureBalance :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSFuture where
  parseNamedRecord m =
    TOSFuture
      <$> m .: "Trade Date"
      <*> m .: "Exec Date"
      <*> m .: "Exec Time"
      <*> m .: "Type"
      <*> m .: "Ref #"
      <*> m .: "Description"
      <*> m .: "Misc Fees"
      <*> m .: "Commissions & Fees"
      <*> m .: "Amount"
      <*> m .: "Balance"

makeLenses ''TOSFuture

data TOSForex = TOSForex
  { _forexDate :: !Text,
    _forexTime :: !Text,
    _forexType :: !Text,
    _forexRefNo :: !Text,
    _forexDescription :: !Text,
    _forexCommissionsAndFees :: !Text,
    _forexAmount :: !Text,
    _forexAmountUSD :: !Text,
    _forexBalance :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSForex where
  parseNamedRecord m =
    TOSForex
      <$> m .: "Date"
      <*> m .: "Time"
      <*> m .: "Type"
      <*> m .: "Ref #"
      <*> m .: "Description"
      <*> m .: "Commissions & Fees"
      <*> m .: "Amount"
      <*> m .: "Amount(USD)"
      <*> m .: "Balance"

makeLenses ''TOSForex

data TOSOrder = TOSOrder
  { _orderNotes :: !Text,
    _orderTimePlaced :: !Text,
    _orderOrderID :: !Text,
    _orderSpread :: !Text,
    _orderSide :: !Text,
    _orderQty :: !Text,
    _orderPosEffect :: !Text,
    _orderSymbol :: !Text,
    _orderExp :: !Text,
    _orderStrike :: !Text,
    _orderType :: !Text,
    _orderPRICE :: !Text,
    _orderTIF :: !Text,
    _orderStatus :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSOrder where
  parseNamedRecord m =
    TOSOrder
      <$> m .: "Notes"
      <*> m .: "Time Placed"
      <*> m .: "Order ID"
      <*> m .: "Spread"
      <*> m .: "Side"
      <*> m .: "Qty"
      <*> m .: "Pos Effect"
      <*> m .: "Symbol"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "PRICE"
      <*> m .: "TIF"
      <*> m .: "Status"

makeLenses ''TOSOrder

data TOSTrade = TOSTrade
  { _tradeExecTime :: !Text,
    _tradeOrderID :: !Text,
    _tradeSpread :: !Text,
    _tradeSide :: !Text,
    _tradeQty :: !Text,
    _tradePosEffect :: !Text,
    _tradeSymbol :: !Text,
    _tradeExp :: !Text,
    _tradeStrike :: !Text,
    _tradeType :: !Text,
    _tradePrice :: !Text,
    _tradeNetPrice :: !Text,
    _tradeOrderType :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSTrade where
  parseNamedRecord m =
    TOSTrade
      <$> m .: "Exec Time"
      <*> m .: "Order ID"
      <*> m .: "Spread"
      <*> m .: "Side"
      <*> m .: "Qty"
      <*> m .: "Pos Effect"
      <*> m .: "Symbol"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "Price"
      <*> m .: "Net Price"
      <*> m .: "Order Type"

makeLenses ''TOSTrade

data TOSEquity = TOSEquity
  { _equitySymbol :: !Text,
    _equityDescription :: !Text,
    _equityQty :: !Text,
    _equityTradePrice :: !Text,
    _equityMark :: !Text,
    _equityMarkValue :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSEquity where
  parseNamedRecord m =
    TOSEquity
      <$> m .: "Symbol"
      <*> m .: "Description"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''TOSEquity

data TOSFuturesOption = TOSFuturesOption
  { _futuresOptionSymbol :: !Text,
    _futuresOptionOptionCode :: !Text,
    _futuresOptionExp :: !Text,
    _futuresOptionStrike :: !Text,
    _futuresOptionType :: !Text,
    _futuresOptionQty :: !Text,
    _futuresOptionTradePrice :: !Text,
    _futuresOptionMark :: !Text,
    _futuresOptionMarkValue :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSFuturesOption where
  parseNamedRecord m =
    TOSFuturesOption
      <$> m .: "Symbol"
      <*> m .: "Option Code"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''TOSFuturesOption

data TOSOption = TOSOption
  { _optionSymbol :: !Text,
    _optionOptionCode :: !Text,
    _optionExp :: !Text,
    _optionStrike :: !Text,
    _optionType :: !Text,
    _optionQty :: !Text,
    _optionTradePrice :: !Text,
    _optionMark :: !Text,
    _optionMarkValue :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSOption where
  parseNamedRecord m =
    TOSOption
      <$> m .: "Symbol"
      <*> m .: "Option Code"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''TOSOption

data TOSProfitAndLoss = TOSProfitAndLoss
  { _pandlSymbol :: !Text,
    _pandlDescription :: !Text,
    _pandlPLOpen :: !Text,
    _pandlPLPerc :: !Text,
    _pandlPLDay :: !Text,
    _pandlPLYTD :: !Text,
    _pandlPLDiff :: !Text,
    _pandlMarginReq :: !Text,
    _pandlMarkValue :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord TOSProfitAndLoss where
  parseNamedRecord m =
    TOSProfitAndLoss
      <$> m .: "Symbol"
      <*> m .: "Description"
      <*> m .: "P/L Open"
      <*> m .: "P/L %"
      <*> m .: "P/L Day"
      <*> m .: "P/L YTD"
      <*> m .: "P/L Diff"
      <*> m .: "Margin Req"
      <*> m .: "Mark Value"

makeLenses ''TOSProfitAndLoss

data ForexAccountSummary = ForexAccountSummary
  { _forexCash :: Amount 2,
    _forexUnrealizedPL :: Amount 2,
    _forexFloatingPL :: Amount 2,
    _forexEquity :: Amount 2,
    _forexMargin :: Amount 2,
    _forexBuyingPower :: Amount 2,
    _forexRiskLevel :: Amount 2,
    _forexCommissionsYTD :: Amount 2
  }
  deriving (Eq, Show)

makeLenses ''ForexAccountSummary

data AccountSummary = AccountSummary
  { _netLiquidatingValue :: Amount 2,
    _stockBuyingPower :: Amount 2,
    _optionBuyingPower :: Amount 2,
    _equityCommissionsFeesYTD :: Amount 2,
    _futuresCommissionsFeesYTD :: Amount 2,
    _totalCommissionsFeesYTD :: Amount 2
  }
  deriving (Eq, Show)

makeLenses ''AccountSummary

data ThinkOrSwim = ThinkOrSwim
  { _account :: Text,
    _name :: Text,
    _since :: Day,
    _until :: Day,
    _xacts :: [TOSTransaction],
    _futures :: [TOSFuture],
    _forex :: [TOSForex],
    _orders :: [TOSOrder],
    _trades :: [TOSTrade],
    _equities :: [TOSEquity],
    _futuresOptions :: [TOSFuturesOption],
    _options :: [TOSOption],
    _profitAndLoss :: [TOSProfitAndLoss],
    _forexSummary :: ForexAccountSummary,
    _accountSummary :: AccountSummary,
    _byOrderId ::
      Map
        Text
        ([TOSTransaction], [TOSOrder], [TOSTrade])
  }
  deriving (Eq, Show)

makeLenses ''ThinkOrSwim

type Symbol = Text

data TOSEntry
  = AchCredit
  | AchDebit
  | AdrFee Symbol
  | Bought TOSDevice TOSTrade'
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
  | RemoveOptionDueToAssignment (Amount 1) Symbol TOSOption'
  | RemoveOptionDueToExpiration (Amount 1) Symbol TOSOption'
  | Sold TOSDevice TOSTrade'
  | TransferBetweenAccounts
  | TransferFromForexAccount
  | TransferInSecurityOrOption (Amount 1) Symbol
  | TransferOfCash
  | TransferToForexAccount
  | WireIncoming
  | Total
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

data TOSOption' = TOSOption'
  { opMult :: Int,
    opEx :: OptionExpirationDate,
    opStrike :: Amount 2,
    opKind :: PutCall
  }
  deriving (Eq)

instance Show TOSOption' where
  show TOSOption' {..} =
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

data TOSFuturesOption' = TOSFuturesOption'
  { futOpMultNum :: Int,
    futOpMultDen :: Int,
    futOpEx :: FutureOptionExpirationDate,
    futOpContract :: Symbol,
    futOpStrike :: Amount 2,
    futOpKind :: PutCall
  }
  deriving (Eq, Show)

data TOSOptionTrade
  = SingleOption TOSOption'
  | OptionStrategy Text [Either Symbol TOSOption']
  | SingleFuturesOption TOSFuturesOption'
  | FuturesOptionStrategy Text [Either Symbol TOSFuturesOption']
  deriving (Eq)

instance Show TOSOptionTrade where
  show (SingleOption opt) = show opt
  show (OptionStrategy _ opts) = show opts
  show (SingleFuturesOption fut) = show fut
  show (FuturesOptionStrategy _ futs) = show futs

data TOSTrade' = TOSTrade'
  { tdQuantity :: Amount 0,
    tdSymbol :: Symbol,
    tdOptDetails :: Maybe TOSOptionTrade,
    tdPrice :: Amount 2,
    tdExchange :: Maybe Text
  }
  deriving (Eq)

instance Show TOSTrade' where
  show TOSTrade' {..} =
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
