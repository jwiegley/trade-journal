{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trade.Provider.ThinkOrSwim.Process (thinkOrSwimEntries) where

import Amount
import Control.Arrow (left)
import Control.Exception
import Control.Lens hiding (Context)
import Data.Coerce
import Data.Foldable
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Printf
import Trade.Journal.Types qualified as Journal
import Trade.Provider.ThinkOrSwim.Parser
import Trade.Provider.ThinkOrSwim.Types

entryTime :: Transaction -> UTCTime
entryTime record =
  case parseTimeM False defaultTimeLocale "%m/%d/%y %H:%M:%S" timeString of
    Nothing -> error $ "Could not parse date/time from " ++ show record
    Just t -> t
  where
    splitString :: (Transaction -> Text) -> Char -> String
    splitString key ch =
      intercalate [ch] $
        map
          (printf "%02d" . (read :: String -> Int) . TL.unpack)
          (TL.split (== ch) (key record))

    timeString =
      concat [splitString _xactDate '/', " ", splitString _xactTime ':']

entryParse :: Transaction -> Either (ParseErrorBundle Text Void) Entry
entryParse xact =
  parse
    (parseEntry (xact ^. xactAmount))
    ""
    (xact ^. xactDescription)

entryToAction ::
  Transaction ->
  Entry ->
  Either String Journal.Entry
entryToAction xact = \case
  Bought _device Trade' {..} ->
    Right $
      Journal.TradeEntry $
        Journal.Trade
          { tradeLot =
              Journal.Lot
                { lotAmount = coerce tdQuantity,
                  lotDetail =
                    Journal.TimePrice
                      { price = coerce tdPrice,
                        time = entryTime xact
                      }
                },
            tradeFees =
              (-(xact ^. xactMiscFees . coerced))
                + (-(xact ^. xactCommissionsAndFees . coerced))
          }
  Sold _device Trade' {..} ->
    Right $
      Journal.TradeEntry $
        Journal.Trade
          { tradeLot =
              Journal.Lot
                { lotAmount = coerce (abs tdQuantity),
                  lotDetail =
                    Journal.TimePrice
                      { price = coerce tdPrice,
                        time = entryTime xact
                      }
                },
            tradeFees =
              (-(xact ^. xactMiscFees . coerced))
                + (-(xact ^. xactCommissionsAndFees . coerced))
          }
  AchCredit ->
    Right $
      Journal.DepositEntry $
        Journal.Deposit (xact ^. xactAmount)
  AchDebit ->
    Right $
      Journal.DepositEntry $
        Journal.Deposit (xact ^. xactAmount)
  -- AdrFee _symbol -> undefined
  -- CashAltInterest _amount _symbol -> undefined
  -- CourtesyAdjustment -> undefined
  {-
    CourtesyCredit ->
      Right $
        annotate $
          Journal.IncomeEntry $
            Journal.Credit (xact ^. xactAmount)
  -}
  -- ForeignTaxWithheld _symbol -> undefined
  -- FundDisbursement -> undefined
  -- IncomingAccountTransfer -> undefined
  {-
  InterestAdjustment ->
    Right $
      annotate $
        Journal.IncomeEntry $
          Journal.Interest (xact ^. xactAmount) Nothing
  InterestIncome sym ->
    Right $
      annotate $
        Journal.IncomeEntry $
          Journal.Interest (xact ^. xactAmount) (Just (TL.toStrict sym))
  -}
  -- MarkToMarket -> undefined
  -- MiscellaneousJournalEntry -> undefined
  -- OffCycleInterest _symbol -> undefined
  -- OrdinaryDividend _symbol -> undefined
  -- QualifiedDividend _symbol -> undefined
  {-
  Rebate ->
    Right $
      annotate $
        Journal.IncomeEntry $
          Journal.Income (xact ^. xactAmount)
  -}
  -- RemoveOptionDueToAssignment _amount _symbol _option -> undefined
  -- RemoveOptionDueToExpiration _amount _symbol _option -> undefined
  -- TransferBetweenAccounts -> undefined
  -- TransferFromForexAccount -> undefined
  {-
  TransferInSecurityOrOption amt sym ->
    Right $
      annotate $
        Journal.DepositEntry $
          Journal.Transfer
            Journal.Lot
              { _amount = coerce amt,
                _symbol = TL.toStrict sym,
                _price = 0
              }
            ""
  -}
  -- TransferOfCash -> undefined
  -- TransferToForexAccount -> undefined
  {-
  WireIncoming ->
    Right $
      annotate $
        Journal.DepositEntry $
          Journal.Deposit (xact ^. xactAmount) ""
  -}
  -- Total -> undefined
  x -> Left $ "Could not convert entry to action: " ++ show x

{-
  where
    lotDetails =
      [ Meta "Order" (TL.toStrict (xact ^. xactRefNo)),
        Note (TL.toStrict (xact ^. xactDescription))
      ]
-}

xactAction ::
  Transaction ->
  Amount 2 ->
  Either String Journal.Entry
xactAction xact bal = do
  ent <- left show $ entryParse xact
  x <- entryToAction xact ent
  -- assert (sum (x ^.. _NetAmount) == xact ^. xactAmount) $
  assert (bal == xact ^. xactBalance) $
    pure x

thinkOrSwimEntries ::
  ThinkOrSwim ->
  [Journal.Entry]
thinkOrSwimEntries tos =
  concatMap
    ( \case
        Left err -> trace err []
        Right x -> [x]
    )
    $ snd
    $ (\f -> foldr' f (0 :: Amount 2, []) (tos ^. xacts))
    $ \xact (bal, rest) ->
      let nxt = bal + xact ^. xactAmount
       in case xactAction xact nxt of
            x@(Left _) -> (bal, x : rest)
            x -> (nxt, x : rest)
