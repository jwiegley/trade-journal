{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- {-# LANGUAGE TemplateHaskell #-}

module Journal.ThinkOrSwim.Process (thinkOrSwimToJournal) where

import Control.Arrow (left)
import Control.Lens
import Data.Coerce
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
-- import Journal.Amount
import Data.Time
import Data.Void (Void)
import Debug.Trace
import Journal.ThinkOrSwim.Parser
import Journal.ThinkOrSwim.Types
import Journal.Types
import Text.Megaparsec
import Text.Printf

entryTime :: TOSTransaction -> UTCTime
entryTime record =
  case parseTimeM False defaultTimeLocale "%m/%d/%y %H:%M:%S" timeString of
    Nothing -> error $ "Could not parse date/time from " ++ show record
    Just t -> t
  where
    splitString :: (TOSTransaction -> Text) -> Char -> String
    splitString key ch =
      intercalate [ch] $
        map
          (printf "%02d" . (read :: String -> Int) . TL.unpack)
          (TL.split (== ch) (key record))

    timeString =
      concat [splitString _xactDate '/', " ", splitString _xactTime ':']

entryParse :: TOSTransaction -> Either (ParseErrorBundle Text Void) TOSEntry
entryParse record =
  parse
    parseEntry
    ""
    (record ^. xactDescription)

entryToAction :: TOSTransaction -> TOSEntry -> Either String Action
entryToAction xact = \case
  Bought _device TOSTrade' {..} ->
    Right $
      Buy
        Lot
          { _amount = coerce tdQuantity,
            _symbol = TL.toStrict tdSymbol,
            _price = coerce tdPrice,
            _details = [], -- jww (2021-05-29): ???
            _computed = []
          }
  Sold _device TOSTrade' {..} ->
    Right $
      Sell
        Lot
          { _amount = coerce (abs tdQuantity),
            _symbol = TL.toStrict tdSymbol,
            _price = coerce tdPrice,
            _details = [], -- jww (2021-05-29): ???
            _computed = []
          }
  -- AchCredit -> undefined
  -- AchDebit -> undefined
  -- AdrFee _symbol -> undefined
  -- CashAltInterest _amount _symbol -> undefined
  -- CourtesyAdjustment -> undefined
  CourtesyCredit ->
    Right $
      Credit (TL.toStrict (xact ^. xactDescription)) (xact ^. xactAmount)
  -- ForeignTaxWithheld _symbol -> undefined
  -- FundDisbursement -> undefined
  -- IncomingAccountTransfer -> undefined
  -- InterestAdjustment -> undefined
  -- InterestIncome _symbol -> undefined
  -- MarkToMarket -> undefined
  -- MiscellaneousJournalEntry -> undefined
  -- OffCycleInterest _symbol -> undefined
  -- OrdinaryDividend _symbol -> undefined
  -- QualifiedDividend _symbol -> undefined
  -- Rebate -> undefined
  -- RemoveOptionDueToAssignment _amount _symbol _option -> undefined
  -- RemoveOptionDueToExpiration _amount _symbol _option -> undefined
  -- TransferBetweenAccounts -> undefined
  -- TransferFromForexAccount -> undefined
  -- TransferInSecurityOrOption -> undefined
  -- TransferOfCash -> undefined
  -- TransferToForexAccount -> undefined
  -- WireIncoming -> undefined
  -- Total -> undefined
  x -> Left $ "Could not convert entry to action: " ++ show x

  -- Wash Lot
  -- Deposit Lot
  -- Withdraw Lot
  -- Assign Lot
  -- Expire Lot
  -- Dividend Lot

xactAction :: TOSTransaction -> Either String (Timed Action)
xactAction record = do
  ent <- left show $ entryParse record
  act <- entryToAction record ent
  pure $
    Timed
      { _time = entryTime record,
        _item = act
      }

thinkOrSwimToJournal :: ThinkOrSwim -> Journal
thinkOrSwimToJournal tos =
  Journal $
    flip concatMap (tos ^. xacts) $ \xact ->
      case xactAction xact of
        Left err -> trace err []
        Right x -> [x]
