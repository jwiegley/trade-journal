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
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Format.ISO8601
import           Prelude hiding (Float, Double)
import           Text.Printf
import           Text.Show.Pretty

renderRefs :: [Ref] -> Text
renderRefs = T.intercalate "," . map go
  where
    go r = case r^.refType of
        WashSaleRule wash ->
            "W$" <> T.pack (show wash) <> "-" <> T.pack (show (r^.refId))
        RollingOrder roll ->
            "R$" <> T.pack (show roll) <> "-" <> T.pack (show (r^.refId))
        OpeningOrder   -> "" <> T.pack (show (r^.refId))
        ExistingEquity -> "Equity"

renderPostingAmount :: PostingAmount k LotAndPL -> [Text]
renderPostingAmount NoAmount = [""]
renderPostingAmount (DollarAmount amt) = ["$" <> T.pack (thousands amt)]
renderPostingAmount (CommodityAmount (LotAndPL _ _ _ l@(CommodityLot {..})))
    = map T.pack
          [ renderAmount _quantity
          , printf "%s%s%s%s%s"
              (if T.all isAlpha _symbol
               then _symbol
               else "\"" <> _symbol <> "\"")
              (maybe "" (T.pack . printf " {$%s}" . thousands . abs)
                        (perShareCost l))
              (maybe "" (T.pack . printf " [%s]" . iso8601Show) _purchaseDate)
              (case _refs of
                   [] -> ""
                   xs -> (T.pack . printf " (%s)" . renderRefs) xs)
              (maybe "" (T.pack . printf " @ $%s" . thousands) _price)
          ]
  where
    perShareCost CommodityLot {..} =
        fmap coerce ((/ _quantity) <$> _cost) :: Maybe (Amount 6)

renderAccount :: Account -> Text
renderAccount = \case
    Equities actId       -> "Assets:TD:" <> actId <> ":Equities"
    Futures actId        -> "Assets:TD:" <> actId <> ":Futures"
    Options actId        -> "Assets:TD:" <> actId <> ":Options"
    FuturesOptions actId -> "Assets:TD:" <> actId <> ":Futures:Options"
    Forex actId          -> "Assets:TD:" <> actId <> ":Forex"
    Cash actId           -> "Assets:TD:" <> actId <> ":Cash"
    Bonds actId          -> "Assets:TD:" <> actId <> ":Bonds"
    MoneyMarkets actId   -> "Assets:TD:" <> actId <> ":MoneyMarkets"
    Fees                 -> "Expenses:TD:Fees"
    Charges              -> "Expenses:TD:Charges"
    Commissions          -> "Expenses:TD:Commission"
    CapitalGainShort     -> "Income:Capital:Short"
    CapitalGainLong      -> "Income:Capital:Long"
    CapitalLossShort     -> "Expenses:Capital:Short"
    CapitalLossLong      -> "Expenses:Capital:Long"
    CapitalWashLoss      -> "Expenses:Capital:Short:Wash"
    RoundingError        -> "Expenses:TD:Rounding"
    OpeningBalances      -> "Equity:TD:Opening Balances"

renderPosting :: Posting k LotAndPL -> [Text]
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

renderTransaction :: Show k => Transaction k o LotAndPL -> [Text]
renderTransaction xact
    = [ T.concat
          $  [ T.pack (iso8601Show (xact^.actualDate)) ]
          ++ maybeToList (T.pack . iso8601Show <$> xact^.effectiveDate)
          ++ [ " * (", xact^.code, ") ", xact^.payee ]
      ]
   ++ renderMetadata (xact^.xactMetadata)
   ++ concatMap doPost (xact^.postings)
  where
    doPost p = capital ++ renderPosting p
      where
        capital = case p^?amount._CommodityAmount of
            Just pl | pl^.plLoss /= 0 ->
                renderPosting
                    (newPosting
                     (fromMaybe
                         (error $ "No account for " ++ ppShow pl)
                         (plAccount (pl^.plKind)))
                     False (DollarAmount (pl^.plLoss)))
            _ -> []
