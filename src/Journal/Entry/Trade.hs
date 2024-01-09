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
import Data.Csv hiding (Parser)
import qualified Data.Csv as Csv
import Data.Sum.Lens
import Data.Text.Lazy
import GHC.Generics hiding (to)
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
    _tradeLot :: Lot
  }
  deriving (Show, PrettyVal, Eq, Generic)

makeLenses ''Trade

instance HasLot (Const Trade) where
  _Lot f (Const s) = fmap Const $ s & tradeLot %%~ f

_TradeNetAmount :: Fold Trade (Amount 2)
_TradeNetAmount f s@Trade {..} =
  Trade _tradeAction _tradeLot _tradeFees
    <$> ( f
            ( ( case _tradeAction of
                  Buy -> negate
                  Sell -> id
              )
                ( totaled
                    _tradeLot
                    (_tradeLot ^. price + sum (s ^.. fees))
                )
                ^. coerced
            )
            <&> view coerced
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

-- <> ( if _tradeFees > 0
--        then " fees " <> totalAmount (Just _tradeLot) 2 _tradeFees
--        else ""
--    )
-- <> ( if _tradeCommission > 0
--        then " commission " <> totalAmount (Just _tradeLot) 2 _tradeCommission
--        else ""
--    )

instance Printable (Const Trade) where
  printItem = printTrade . getConst

parseTrade :: Parser Trade
parseTrade = do
  _tradeAction <- (Buy <$ keyword "buy") <|> (Sell <$ keyword "sell")
  _tradeLot <- parseLot
  -- _tradeFees <-
  --   ( keyword "fees"
  --       *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
  --     )
  --     <|> pure 0
  -- _tradeCommission <-
  --   ( keyword "commission"
  --       *> ((/ (_tradeLot ^. amount)) <$> parseAmount)
  --     )
  --     <|> pure 0
  pure Trade {..}

instance Producible Parser (Const Trade) where
  produce = fmap Const parseTrade

newtype CsvParser a = CsvParser
  { runCsvParser :: NamedRecord -> Csv.Parser a
  }

csvParseTrade :: CsvParser Trade
csvParseTrade = CsvParser $ \m -> do
  _tradeAction <- m .: "action"
  _tradeLot <- parseLot =<< m .: "lot"
  -- _tradeFees <- m .: "fees"
  -- _tradeCommission <- m .: "commission"
  pure Trade {..}

class HasCsvParser a where
  csvParser :: CsvParser a

instance HasCsvParser Trade where
  csvParser = csvParseTrade

instance HasCsvParser a => FromNamedRecord a where
  parseNamedRecord = runCsvParser (csvParser @a)

instance Producible CsvParser (Const Trade) where
  produce = fmap Const csvParseTrade
