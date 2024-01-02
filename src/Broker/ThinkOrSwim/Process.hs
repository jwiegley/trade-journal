{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Broker.ThinkOrSwim.Process (thinkOrSwimEntries) where

import Amount
import Broker.ThinkOrSwim.Parser
import Broker.ThinkOrSwim.Types
import Control.Arrow (left)
import Control.Exception
import Control.Lens hiding (Context)
import Data.Coerce
import Data.Foldable
import Data.List (intercalate)
import Data.Sum
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Void (Void)
import Debug.Trace
import Journal.Entry as Journal
import Journal.Types
import Text.Megaparsec
import Text.Printf

type Event =
  Sum '[Const Journal.Trade, Const Deposit, Const Income, Const Options] ()

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
  Context ->
  Transaction ->
  Entry ->
  Either String (Annotated Event)
entryToAction ctx xact = \case
  Bought _device Trade' {..} ->
    Right $
      annotate $
        inject $
          Const $
            Journal.Trade
              { _tradeAction = Buy,
                _tradeLot =
                  Lot
                    { _amount = coerce tdQuantity,
                      _symbol = TL.toStrict tdSymbol,
                      _price = coerce tdPrice
                    },
                _tradeFees =
                  Fees
                    (-(xact ^. xactMiscFees . coerced))
                    (-(xact ^. xactCommissionsAndFees . coerced))
              }
  Sold _device Trade' {..} ->
    Right $
      annotate $
        inject $
          Const $
            Journal.Trade
              { _tradeAction = Sell,
                _tradeLot =
                  Lot
                    { _amount = coerce (abs tdQuantity),
                      _symbol = TL.toStrict tdSymbol,
                      _price = coerce tdPrice
                    },
                _tradeFees =
                  Fees
                    (-(xact ^. xactMiscFees . coerced))
                    (-(xact ^. xactCommissionsAndFees . coerced))
              }
  AchCredit ->
    Right $
      annotate $
        inject $
          Const $
            Deposit (xact ^. xactAmount) ""
  AchDebit ->
    Right $
      annotate $
        inject $
          Const $
            Deposit (xact ^. xactAmount) ""
  -- AdrFee _symbol -> undefined
  -- CashAltInterest _amount _symbol -> undefined
  -- CourtesyAdjustment -> undefined
  CourtesyCredit ->
    Right $
      annotate $
        inject $
          Const $
            Credit (xact ^. xactAmount)
  -- ForeignTaxWithheld _symbol -> undefined
  -- FundDisbursement -> undefined
  -- IncomingAccountTransfer -> undefined
  InterestAdjustment ->
    Right $
      annotate $
        inject $
          Const $
            Interest (xact ^. xactAmount) Nothing
  InterestIncome sym ->
    Right $
      annotate $
        inject $
          Const $
            Interest (xact ^. xactAmount) (Just (TL.toStrict sym))
  -- MarkToMarket -> undefined
  -- MiscellaneousJournalEntry -> undefined
  -- OffCycleInterest _symbol -> undefined
  -- OrdinaryDividend _symbol -> undefined
  -- QualifiedDividend _symbol -> undefined
  Rebate ->
    Right $
      annotate $
        inject $
          Const $
            Income (xact ^. xactAmount)
  -- RemoveOptionDueToAssignment _amount _symbol _option -> undefined
  -- RemoveOptionDueToExpiration _amount _symbol _option -> undefined
  -- TransferBetweenAccounts -> undefined
  -- TransferFromForexAccount -> undefined
  TransferInSecurityOrOption amt sym ->
    Right $
      annotate $
        inject $
          Const $
            Transfer
              Lot
                { _amount = coerce amt,
                  _symbol = TL.toStrict sym,
                  _price = 0
                }
              ""
  -- TransferOfCash -> undefined
  -- TransferToForexAccount -> undefined
  WireIncoming ->
    Right $
      annotate $
        inject $
          Const $
            Deposit (xact ^. xactAmount) ""
  -- Total -> undefined
  x -> Left $ "Could not convert entry to action: " ++ show x
  where
    annotate x =
      Annotated
        { _item = x,
          _time = entryTime xact,
          _context = ctx,
          _details = lotDetails
        }
    lotDetails =
      [ Meta "Order" (TL.toStrict (xact ^. xactRefNo)),
        Note (TL.toStrict (xact ^. xactDescription))
      ]

xactAction ::
  Context ->
  Transaction ->
  Amount 2 ->
  Either String (Annotated Event)
xactAction ctx xact bal = do
  ent <- left show $ entryParse xact
  x <- entryToAction ctx xact ent
  assert (sum (x ^.. item . _NetAmount) == xact ^. xactAmount) $
    assert (bal == xact ^. xactBalance) $
      pure x

thinkOrSwimEntries ::
  Context ->
  ThinkOrSwim ->
  [Annotated Event]
thinkOrSwimEntries ctx tos =
  concatMap
    ( \case
        Left err -> trace err []
        Right x -> [x]
    )
    $ snd
    $ (\f -> foldr' f (0 :: Amount 2, []) (tos ^. xacts))
    $ \xact (bal, rest) ->
      let nxt = bal + xact ^. xactAmount
       in case xactAction ctx xact nxt of
            x@(Left _) -> (bal, x : rest)
            x -> (nxt, x : rest)
