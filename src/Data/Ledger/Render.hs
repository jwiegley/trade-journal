{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Ledger.Render where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Amount
import           Data.Char (isAlpha)
import           Data.Coerce
import           Data.Ledger
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double)
import           Text.Printf

renderPostingAmount :: PostingAmount k CommodityLot -> [Text]
renderPostingAmount NoAmount = [""]
renderPostingAmount (DollarAmount amt) = ["$" <> T.pack (thousands amt)]
renderPostingAmount (CommodityAmount l@(CommodityLot {..}))
    = map T.pack
          [ renderAmount _quantity
          , printf "%s%s%s%s%s"
              (if T.all isAlpha _symbol
               then _symbol
               else "\"" <> _symbol <> "\"")
              (maybe "" (T.pack . printf " {$%s}" . thousands . abs)
                        (perShareCost l))
              (maybe "" (T.pack . printf " [%s]" . iso8601Show) _purchaseDate)
              (case _note of
                   Nothing -> ""
                   Just txt -> (T.pack . printf " (%s)") txt)
              (maybe "" (T.pack . printf " @ $%s" . thousands) _price)
          ]
  where
    perShareCost CommodityLot {..} =
        fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)

renderAccount :: Account -> Text
renderAccount = \case
    Equities actId          -> "Assets:TD:" <> actId <> ":Equities"
    Futures actId           -> "Assets:TD:" <> actId <> ":Futures"
    Options actId           -> "Assets:TD:" <> actId <> ":Options"
    FuturesOptions actId    -> "Assets:TD:" <> actId <> ":Futures:Options"
    Forex actId             -> "Assets:TD:" <> actId <> ":Forex"
    Cash actId              -> "Assets:TD:" <> actId <> ":Cash"
    Bonds actId             -> "Assets:TD:" <> actId <> ":Bonds"
    MoneyMarkets actId      -> "Assets:TD:" <> actId <> ":MoneyMarkets"
    Fees                    -> "Expenses:TD:Fees"
    Charges                 -> "Expenses:TD:Charges"
    Commissions             -> "Expenses:TD:Commission"
    CapitalGainLong         -> "Income:Capital:Long"
    CapitalGainShort        -> "Income:Capital:Short"
    CapitalLossLong         -> "Expenses:Capital:Long"
    CapitalLossShort        -> "Expenses:Capital:Short"
    CapitalWashLoss         -> "Expenses:Capital:Wash"
    CapitalWashLossDeferred -> "Expenses:Capital:Wash:Deferred"
    RoundingError           -> "Expenses:TD:Rounding"
    OpeningBalances         -> "Equity:TD:Opening Balances"
    Unknown                 -> "Unknown"

renderPosting :: Posting k CommodityLot -> [Text]
renderPosting Posting {..} =
    [ T.pack $ printf "    %-32s%16s%s"
        (if _isVirtual then "(" <> act <> ")" else act)
        (head xs)
        (case xs of _:y:_ -> " " <> y; _ -> "")
    ]
    ++ renderMetadata _postMetadata
  where
    act = renderAccount _account
    xs  = renderPostingAmount _amount

renderMetadata :: Map Text Text -> [Text]
renderMetadata = Prelude.map go . M.assocs
  where
    go (k, v) = "    ; " <> k <> ": " <> v

renderTransaction :: Show k => Transaction k o CommodityLot -> [Text]
renderTransaction xact
    = [ T.concat
          $  [ T.pack (iso8601Show (xact^.actualDate)) ]
          ++ maybeToList (T.pack . iso8601Show <$> xact^.effectiveDate)
          ++ [ " * (", xact^.code, ") ", xact^.payee ]
      ]
   ++ renderMetadata (xact^.xactMetadata)
   ++ concatMap renderPosting (xact^.postings)
