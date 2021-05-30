{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.ThinkOrSwim.Types where

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Journal.Amount

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
    _xacts :: [Csv.NamedRecord],
    _futures :: [Csv.NamedRecord],
    _forex :: [Csv.NamedRecord],
    _orders :: [Csv.NamedRecord],
    _trades :: [Csv.NamedRecord],
    _equities :: [Csv.NamedRecord],
    _futuresOptions :: [Csv.NamedRecord],
    _options :: [Csv.NamedRecord],
    _profitAndLoss :: [Csv.NamedRecord],
    _forexSummary :: ForexAccountSummary,
    _accountSummary :: AccountSummary,
    _byOrderId ::
      Map
        B.ByteString
        ([Csv.NamedRecord], [Csv.NamedRecord], [Csv.NamedRecord])
  }
  deriving (Eq, Show)

makeLenses ''ThinkOrSwim

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
