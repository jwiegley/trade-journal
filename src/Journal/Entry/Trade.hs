{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Sum.Lens
import Data.Text.Lazy
import GHC.Generics hiding (to)
import Journal.Entry.Fees
import Journal.Parse
import Journal.Print
import Journal.Types.Entry
import Journal.Types.Lot
import Text.Show.Pretty
import Prelude hiding (Double, Float)

data Action = Buy | Sell
  deriving (Show, PrettyVal, Eq, Ord, Generic)

data Trade = Trade
  { _tradeAction :: Action,
    _tradeLot :: Lot,
    _tradeFees :: Fees
  }
  deriving (Show, PrettyVal, Eq, Generic)

makeLenses ''Trade

tradeTotalFees :: Fold Trade (Amount 6)
tradeTotalFees f Trade {..} =
  Trade _tradeAction _tradeLot <$> totalFees f _tradeFees

_TradeLot :: Traversal' Trade Lot
_TradeLot f s = s & tradeLot %%~ f

instance HasLot (Const Trade) where
  _Lot f (Const s) = fmap Const $ s & _TradeLot %%~ f

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

instance HasNetAmount (Const Trade) where
  _NetAmount f (Const s) = fmap Const $ s & _TradeNetAmount %%~ f

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

instance Printable (Const Trade) where
  printItem = printTrade . getConst

parseTrade :: Parser Trade
parseTrade = do
  _tradeAction <- (Buy <$ keyword "buy") <|> (Sell <$ keyword "sell")
  _tradeLot <- parseLot
  _tradeFees <-
    Fees
      <$> ( ( keyword "fees"
                *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
            )
              <|> pure 0
          )
      <*> ( ( keyword "commission"
                *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
            )
              <|> pure 0
          )
  pure Trade {..}

instance Producible Parser (Const Trade) where
  produce = fmap Const parseTrade
