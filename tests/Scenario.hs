{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module provides a DSL for simulating trading actions, and then
-- calculates the final set of Ledger transactions that would result, which
-- can be checked against constructions using another DSL.

module Scenario where

import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Int (Int64)
-- import           Data.Ledger as Ledger
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API

data Scenario = Scenario
    { _xacts  :: [API.Transaction]
    , _nextId :: Int64
    , _today  :: Maybe Text
    }
    deriving (Eq, Show)

makeClassy ''Scenario

newScenario :: Scenario
newScenario = Scenario
    { _xacts  = []
    , _nextId = 1
    , _today  = Nothing
    }

type Transactions a = State Scenario a

date :: Text -> Transactions a -> Transactions a
date isoDate action = do
    today ?= isoDate
    res <- action
    today .= Nothing
    pure res

order :: Text -> Transactions a -> Transactions a
order name action = do
    md     <- use today
    before <- use xacts
    xacts .= []
    res <- action
    xacts.traverse %= \x -> do
        x & transactionOrderId   ?~ name
          & transactionOrderDate .~ fmap (\d -> dayToUtc d "12:00:00") md
    xacts %= (before ++)
    pure res

dayToUtc :: Text -> Text -> UTCTime
dayToUtc dy tm =
    let dt = T.unpack $ dy <> "T" <> tm <> "Z"
    in case iso8601ParseM dt of
           Nothing -> error $ "Date/time invalid: " ++ dt
           Just m  -> m

-- dated "2020-03-15" $ do
--     open  100 "AAPL" 235.44 "12:34:30"  23544.00
--     close 100 "AAPL" 234.50 "12:34:30"  23544.00
open :: Amount 6 -> Text -> Amount 6 -> Text -> Amount 2 -> Transactions ()
open n name per time total = do
    nid <- use nextId
    nextId += 1
    md <- use today
    case md of
        Nothing -> error "Must set current day using 'date'"
        Just d  -> xacts %= (t nid (dayToUtc d time):)
  where
    t nid moment = API.Transaction
        { _transactionInfo_ = TransactionInfo
            { _transactionId      = nid
            , _transactionSubType = if n < 0 then SellTrade else BuyTrade
            , _transactionDate    = moment
            , _transactionItem_ = TransactionItem
                { _transactionInstrument = Just $ API.Instrument
                    { _assetType  =
                      if T.isInfixOf "_" name
                      then API.OptionAsset Option
                          { _description      = "DESCRIPTION"
                          , _putCall          = Put
                          , _strikePrice      = Nothing
                          , _expirationDate   = undefined
                          , _underlyingSymbol = ""
                          }
                      else API.Equity
                    , API._symbol = name
                    , _cusip      = name
                    }
                , _positionEffect = Just Open
                , _instruction    = Just $ if n < 0 then Sell else Buy
                , API._cost       = n * per
                , API._price      = Just per
                , API._amount     = Just n
                , _accountId      = 123
                }

            , _transactionDescription = "DESCRIPTION"
            }

        , _fees_ = API.Fees
            { _rFee          = 0.0
            , _additionalFee = 0.0
            , _cdscFee       = 0.0
            , _regFee        = 0.0
            , _otherCharges  = 0.0
            , _commission    = 0.0
            , _optRegFee     = 0.0
            , _secFee        = 0.0
            }

        , _accruedInterest               = Nothing
        , _achStatus                     = Nothing
        , _cashBalanceEffectFlag         = True
        , _transactionOrderDate          = Nothing
        , _netAmount                     = total
        , _dayTradeBuyingPowerEffect     = Nothing
        , _requirementReallocationAmount = Nothing
        , _sma                           = Nothing
        , _transactionOrderId            = Nothing -- jww (2020-04-19): NYI
        , _settlementDate                = utctDay moment
        , _subAccount                    = "1"
        , _clearingReferenceNumber       = Nothing
        , _type_                         = Trade
        }
