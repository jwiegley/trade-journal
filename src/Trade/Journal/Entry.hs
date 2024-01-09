{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Journal.Entry (module X, Entry (..), AsEntry (..)) where

import Control.Lens
import Data.Data
import GHC.Generics hiding (to)
import Text.Show.Pretty hiding (Time)
import Trade.Journal.Entry.Deposit as X
import Trade.Journal.Entry.Fees as X
import Trade.Journal.Entry.Income as X
import Trade.Journal.Entry.OptionTrade as X
import Trade.Journal.Entry.Trade as X
import Trade.Journal.Print
import Trade.Journal.Types.Entry as X
import Trade.Journal.Types.Lot as X

data Entry
  = TradeEntry !Trade
  | OptionTradeEntry !OptionTrade
  | IncomeEntry !Income
  | DepositEntry !Deposit
  deriving (Show, Eq, PrettyVal, Generic, Data)

makeClassyPrisms ''Entry

instance Printable Entry where
  printItem (TradeEntry s) = printItem s
  printItem (OptionTradeEntry s) = printItem s
  printItem (IncomeEntry s) = printItem s
  printItem (DepositEntry s) = printItem s

instance HasNetAmount Entry where
  _NetAmount f (TradeEntry s) = TradeEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (OptionTradeEntry s) = OptionTradeEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (IncomeEntry s) = IncomeEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (DepositEntry s) = DepositEntry <$> (s & _NetAmount %%~ f)

instance HasLot Entry where
  _Lot f (TradeEntry s) = TradeEntry <$> (s & _Lot %%~ f)
  _Lot f (OptionTradeEntry s) = OptionTradeEntry <$> (s & _Lot %%~ f)
  _Lot f (IncomeEntry s) = IncomeEntry <$> (s & _Lot %%~ f)
  _Lot f (DepositEntry s) = DepositEntry <$> (s & _Lot %%~ f)
