{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Trade.Journal.Entry.OptionTrade where

import Amount
import Control.Lens
import Data.Data
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Text.Show.Pretty
import Trade.Journal.Print
import Trade.Journal.Types.Entry
import Trade.Journal.Types.Lot
import Prelude hiding (Double, Float)

data OptionAction
  = Exercise -- exercise a long options position
  | Assign -- assignment of a short options position
  | Expire -- expiration of a short options position
  deriving (Show, PrettyVal, Eq, Ord, Generic, Data)

-- | An Event represents "internal events" that occur within an account,
--   either directly due to the actions above, or indirectly because of other
--   factors.
data OptionTrade = OptionTrade
  { _optionTradeAction :: !OptionAction,
    _optionTradeLot :: !Lot
  }
  deriving (Show, PrettyVal, Eq, Generic, Data)

makeLenses ''OptionTrade

instance HasLot OptionTrade where
  _Lot f s = s & optionTradeLot %%~ f

_OptionTradeNetAmount :: Fold OptionTrade (Amount 2)
_OptionTradeNetAmount f =
  error "impossible" . f . const 0 -- jww (2021-06-12): NYI

instance HasNetAmount OptionTrade where
  _NetAmount f s = s & _OptionTradeNetAmount %%~ f

printOptionTrade :: OptionTrade -> TL.Text
printOptionTrade OptionTrade {..} =
  ( case _optionTradeAction of
      Exercise -> "exercise "
      Assign -> "assign "
      Expire -> "expire "
  )
    <> printLot _optionTradeLot

instance Printable OptionTrade where
  printItem = printOptionTrade
