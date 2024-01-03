{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Journal.Entry (module X, Entry (..), AsEntry (..)) where

import Control.Lens
import GHC.Generics hiding (to)
import Journal.Entry.Deposit as X
import Journal.Entry.Fees as X
import Journal.Entry.Income as X
import Journal.Entry.Options as X
import Journal.Entry.Trade as X
import Journal.Print
import Journal.Types.Entry as X
import Journal.Types.Lot as X
import Text.Show.Pretty hiding (Time)

data Entry
  = TradeEntry !Trade
  | OptionsEntry !Options
  | IncomeEntry !Income
  | DepositEntry !Deposit
  deriving (Show, Eq, PrettyVal, Generic)

makeClassyPrisms ''Entry

instance Printable Entry where
  printItem (TradeEntry s) = printItem s
  printItem (OptionsEntry s) = printItem s
  printItem (IncomeEntry s) = printItem s
  printItem (DepositEntry s) = printItem s

instance HasNetAmount Entry where
  _NetAmount f (TradeEntry s) = TradeEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (OptionsEntry s) = OptionsEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (IncomeEntry s) = IncomeEntry <$> (s & _NetAmount %%~ f)
  _NetAmount f (DepositEntry s) = DepositEntry <$> (s & _NetAmount %%~ f)

instance HasLot Entry where
  _Lot f (TradeEntry s) = TradeEntry <$> (s & _Lot %%~ f)
  _Lot f (OptionsEntry s) = OptionsEntry <$> (s & _Lot %%~ f)
  _Lot f (IncomeEntry s) = IncomeEntry <$> (s & _Lot %%~ f)
  _Lot f (DepositEntry s) = DepositEntry <$> (s & _Lot %%~ f)
