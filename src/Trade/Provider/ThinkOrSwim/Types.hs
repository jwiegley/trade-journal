{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Provider.ThinkOrSwim.Types where

import Amount
import Control.Lens
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Text.Read (readMaybe)

data Transaction = Transaction
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

readAmount :: KnownNat n => String -> Amount n
readAmount "" = 0
readAmount ('(' : xs) = -(readAmount xs)
readAmount s = case readMaybe (filter (`notElem` [',', '$', ')']) s) of
  Nothing -> error $ "Failed to read amount: " ++ s
  Just x -> x

instance Csv.FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> m .: "DATE"
      <*> m .: "TIME"
      <*> m .: "TYPE"
      <*> m .: "REF #"
      <*> m .: "DESCRIPTION"
      <*> (readAmount <$> m .: "Misc Fees")
      <*> (readAmount <$> m .: "Commissions & Fees")
      <*> (readAmount <$> m .: "AMOUNT")
      <*> (readAmount <$> m .: "BALANCE")

makeLenses ''Transaction

data Future = Future
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

instance Csv.FromNamedRecord Future where
  parseNamedRecord m =
    Future
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

makeLenses ''Future

data Forex = Forex
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

instance Csv.FromNamedRecord Forex where
  parseNamedRecord m =
    Forex
      <$> m .: "Date"
      <*> m .: "Time"
      <*> m .: "Type"
      <*> m .: "Ref #"
      <*> m .: "Description"
      <*> m .: "Commissions & Fees"
      <*> m .: "Amount"
      <*> m .: "Amount(USD)"
      <*> m .: "Balance"

makeLenses ''Forex

data Order = Order
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

instance Csv.FromNamedRecord Order where
  parseNamedRecord m =
    Order
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

makeLenses ''Order

data Trade = Trade
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

instance Csv.FromNamedRecord Trade where
  parseNamedRecord m =
    Trade
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

makeLenses ''Trade

data Equity = Equity
  { _equitySymbol :: !Text,
    _equityDescription :: !Text,
    _equityQty :: !Text,
    _equityTradePrice :: !Text,
    _equityMark :: !Text,
    _equityMarkValue :: !Text
  }
  deriving (Generic, Eq, Show)

instance Csv.FromNamedRecord Equity where
  parseNamedRecord m =
    Equity
      <$> m .: "Symbol"
      <*> m .: "Description"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''Equity

data FuturesOption = FuturesOption
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

instance Csv.FromNamedRecord FuturesOption where
  parseNamedRecord m =
    FuturesOption
      <$> m .: "Symbol"
      <*> m .: "Option Code"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''FuturesOption

data Option = Option
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

instance Csv.FromNamedRecord Option where
  parseNamedRecord m =
    Option
      <$> m .: "Symbol"
      <*> m .: "Option Code"
      <*> m .: "Exp"
      <*> m .: "Strike"
      <*> m .: "Type"
      <*> m .: "Qty"
      <*> m .: "Trade Price"
      <*> m .: "Mark"
      <*> m .: "Mark Value"

makeLenses ''Option

data ProfitAndLoss = ProfitAndLoss
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

instance Csv.FromNamedRecord ProfitAndLoss where
  parseNamedRecord m =
    ProfitAndLoss
      <$> m .: "Symbol"
      <*> m .: "Description"
      <*> m .: "P/L Open"
      <*> m .: "P/L %"
      <*> m .: "P/L Day"
      <*> m .: "P/L YTD"
      <*> m .: "P/L Diff"
      <*> m .: "Margin Req"
      <*> m .: "Mark Value"

makeLenses ''ProfitAndLoss

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
    _xacts :: [Transaction],
    _futures :: [Future],
    _forex :: [Forex],
    _orders :: [Order],
    _trades :: [Trade],
    _equities :: [Equity],
    _futuresOptions :: [FuturesOption],
    _options :: [Option],
    _profitAndLoss :: [ProfitAndLoss],
    _forexSummary :: ForexAccountSummary,
    _accountSummary :: AccountSummary,
    _byOrderId ::
      Map
        Text
        ([Transaction], [Order], [Trade])
  }
  deriving (Eq, Show)

makeLenses ''ThinkOrSwim

type Symbol = Text

data Entry
  = AchCredit
  | AchDebit
  | AdrFee Symbol
  | Bought Device Trade'
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
  | RemoveOptionDueToAssignment (Amount 1) Symbol Option'
  | RemoveOptionDueToExpiration (Amount 1) Symbol Option'
  | Sold Device Trade'
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

data Device = Desktop | IPhone | IPad | Keys [Text] Text
  deriving (Eq, Show)

data OptionExpirationDate = OptionExpirationDate
  { expirationDay :: Day,
    expirationName :: Text
  }
  deriving (Eq, Show)

data Option' = Option'
  { opMult :: Int,
    opEx :: OptionExpirationDate,
    opStrike :: Amount 2,
    opKind :: PutCall
  }
  deriving (Eq)

instance Show Option' where
  show Option' {..} =
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

data FuturesOption' = FuturesOption'
  { futOpMultNum :: Int,
    futOpMultDen :: Int,
    futOpEx :: FutureOptionExpirationDate,
    futOpContract :: Symbol,
    futOpStrike :: Amount 2,
    futOpKind :: PutCall
  }
  deriving (Eq, Show)

data OptionTrade
  = SingleOption Option'
  | OptionStrategy Text [Either Symbol Option']
  | SingleFuturesOption FuturesOption'
  | FuturesOptionStrategy Text [Either Symbol FuturesOption']
  deriving (Eq)

instance Show OptionTrade where
  show (SingleOption opt) = show opt
  show (OptionStrategy _ opts) = show opts
  show (SingleFuturesOption fut) = show fut
  show (FuturesOptionStrategy _ futs) = show futs

data Trade' = Trade'
  { tdQuantity :: Amount 0,
    tdSymbol :: Symbol,
    tdOptDetails :: Maybe OptionTrade,
    tdPrice :: Amount 2,
    tdExchange :: Maybe Text
  }
  deriving (Eq)

instance Show Trade' where
  show Trade' {..} =
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
