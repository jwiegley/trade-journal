{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Broker.ThinkOrSwim.Process (thinkOrSwimEntries) where

import Amount
import Broker.ThinkOrSwim.Parser
import Broker.ThinkOrSwim.Types
import Control.Arrow (left)
import Control.Exception
import Control.Lens hiding (each)
import Data.Coerce
import Data.Foldable
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Data.Void (Void)
import Debug.Trace
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
entryParse xact =
  parse
    (parseEntry (xact ^. xactAmount))
    ""
    (xact ^. xactDescription)

entryToAction ::
  TOSTransaction ->
  TOSEntry ->
  Either String (Annotated Entry)
entryToAction xact = \case
  Bought _device TOSTrade' {..} ->
    Right $
      annotate $
        Trade
          TradeEntry
            { _tradeLot =
                Lot
                  { _amount = coerce tdQuantity,
                    _symbol = TL.toStrict tdSymbol,
                    _price = coerce tdPrice
                  }
            }
  Sold _device TOSTrade' {..} ->
    Right $
      annotate $
        Trade
          TradeEntry
            { _tradeLot =
                Lot
                  { _amount = coerce (abs tdQuantity),
                    _symbol = TL.toStrict tdSymbol,
                    _price = coerce tdPrice
                  }
            }
  AchCredit ->
    Right $
      annotate $
        Deposit (xact ^. xactAmount)
  AchDebit ->
    Right $
      annotate $
        Withdraw (xact ^. xactAmount . to abs)
  -- AdrFee _symbol -> undefined
  -- CashAltInterest _amount _symbol -> undefined
  -- CourtesyAdjustment -> undefined
  CourtesyCredit ->
    Right $
      annotate $
        Credit (xact ^. xactAmount)
  -- ForeignTaxWithheld _symbol -> undefined
  -- FundDisbursement -> undefined
  -- IncomingAccountTransfer -> undefined
  InterestAdjustment ->
    Right $
      annotate $
        Interest (xact ^. xactAmount) Nothing
  InterestIncome sym ->
    Right $
      annotate $
        Interest (xact ^. xactAmount) (Just (TL.toStrict sym))
  -- MarkToMarket -> undefined
  -- MiscellaneousJournalEntry -> undefined
  -- OffCycleInterest _symbol -> undefined
  -- OrdinaryDividend _symbol -> undefined
  -- QualifiedDividend _symbol -> undefined
  Rebate ->
    Right $
      annotate $
        Income (xact ^. xactAmount)
  -- RemoveOptionDueToAssignment _amount _symbol _option -> undefined
  -- RemoveOptionDueToExpiration _amount _symbol _option -> undefined
  -- TransferBetweenAccounts -> undefined
  -- TransferFromForexAccount -> undefined
  TransferInSecurityOrOption amt sym ->
    Right $
      annotate $
        TransferIn
          Lot
            { _amount = coerce (abs amt),
              _symbol = TL.toStrict sym,
              _price = 0
            }
  -- TransferOfCash -> undefined
  -- TransferToForexAccount -> undefined
  WireIncoming ->
    Right $
      annotate $
        Deposit (xact ^. xactAmount)
  -- Total -> undefined
  x -> Left $ "Could not convert entry to action: " ++ show x
  where
    annotate x =
      Annotated
        { _item = x,
          _time = entryTime xact,
          _details = lotDetails
        }
    lotDetails =
      [ Meta "Order" (TL.toStrict (xact ^. xactRefNo)),
        Note (TL.toStrict (xact ^. xactDescription))
      ]

-- ++ [ Fees (- (xact ^. xactMiscFees . coerced))
--      | xact ^. xactMiscFees /= 0
--    ]
-- ++ [ Commission (- (xact ^. xactCommissionsAndFees . coerced))
--      | xact ^. xactCommissionsAndFees /= 0
--    ]

xactAction ::
  TOSTransaction ->
  Amount 2 ->
  Either String (Annotated Entry)
xactAction xact bal = do
  ent <- left show $ entryParse xact
  x <- entryToAction xact ent
  assert (sum (x ^.. item . _EntryNetAmount) == xact ^. xactAmount) $
    assert (bal == xact ^. xactBalance) $
      pure x

thinkOrSwimEntries ::
  ThinkOrSwim ->
  [Annotated Entry]
thinkOrSwimEntries tos =
  concatMap
    ( \case
        Left err -> trace err []
        Right x -> [x]
    )
    $ snd $
      (\f -> foldr' f (0 :: Amount 2, []) (tos ^. xacts)) $
        \xact (bal, rest) ->
          let nxt = bal + xact ^. xactAmount
           in case xactAction xact nxt of
                x@(Left _) -> (bal, x : rest)
                x -> (nxt, x : rest)
