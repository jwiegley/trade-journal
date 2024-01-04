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

module Journal.Entry.Trade where

import Amount
import Control.Applicative
import Control.Lens
import Data.Data
import Data.Text.Lazy
import GHC.Generics hiding (to)
import Journal.Entry.Fees
import Journal.Print
import Journal.Types.Entry
import Journal.Types.Lot
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Action = Buy | Sell
  deriving (Show, PrettyVal, Eq, Ord, Generic, Data)

data Trade = Trade
  { _tradeAction :: !Action,
    _tradeLot :: !Lot,
    _tradeFees :: !Fees
  }
  deriving (Show, PrettyVal, Eq, Generic, Data)

makeLenses ''Trade

tradeTotalFees :: Fold Trade (Amount 6)
tradeTotalFees f Trade {..} =
  Trade _tradeAction _tradeLot <$> totalFees f _tradeFees

_TradeLot :: Traversal' Trade Lot
_TradeLot f s = s & tradeLot %%~ f

instance HasLot Trade where
  _Lot f s = s & _TradeLot %%~ f

_TradeNetAmount :: Fold Trade (Amount 2)
_TradeNetAmount f s@Trade {..} =
  Trade _tradeAction _tradeLot
    <$> ( f
            ( ( case _tradeAction of
                  Buy -> negate
                  Sell -> id
              )
                ( totaled
                    _tradeLot
                    (_tradeLot ^. price + sum (s ^.. tradeTotalFees))
                )
                ^. coerced
            )
            <&> flip Fees 0 . view coerced
        )

instance HasNetAmount Trade where
  _NetAmount f s = s & _TradeNetAmount %%~ f

printTrade :: Trade -> Text
printTrade Trade {..} =
  ( case _tradeAction of
      Buy -> "buy "
      Sell -> "sell "
  )
    <> printLot _tradeLot
    <> ( if _fees _tradeFees > 0
           then " fees " <> totalAmount (Just _tradeLot) 2 (_fees _tradeFees)
           else ""
       )
    <> ( if _commission _tradeFees > 0
           then
             " commission "
               <> totalAmount (Just _tradeLot) 2 (_commission _tradeFees)
           else ""
       )

instance Printable Trade where
  printItem = printTrade
