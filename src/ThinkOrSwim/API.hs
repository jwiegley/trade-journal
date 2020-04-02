{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ThinkOrSwim.API where

import Data.Proxy
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
-- import Network.URI.Encode as URI
import Servant.API
import Servant.Client
import ThinkOrSwim.API.TransactionHistory.GetTransactions

-- HTTP Resource Error Codes
--
-- Code  Description
--
-- 400   An error message indicating the validation problem with the request.
-- 401   An error message indicating the caller must pass a valid Authorization
--       in the HTTP authorization request header.
-- 403   An error message indicating the caller doesn't have access to the
--       account in the request.
-- 500   An error message indicating there was an unexpected server error.
-- 503   An error message indicating there is a temporary problem responding.

baseUrl :: Text
baseUrl = "https://api.tdameritrade.com/v1/"

newtype AccessToken = AccessToken Text

instance ToHttpApiData AccessToken where
    toUrlPiece (AccessToken t) = "Bearer " <> t
    toHeader (AccessToken t) = T.encodeUtf8 $ "Bearer " <> t

newtype AccountId = AccountId Text

instance ToHttpApiData AccountId where
    toUrlPiece (AccountId t) = t

newtype Symbol = Symbol Text

instance ToHttpApiData Symbol where
    toUrlPiece (Symbol t) = t

newtype ISO8601Date = ISO8601Date UTCTime

instance ToHttpApiData ISO8601Date where
    toUrlPiece (ISO8601Date s) =
        T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" s

data TransactionQueryType
    = All
    | Trade_
    | BuyOnly
    | SellOnly
    | CashInOrCashOut
    | Checking
    | Dividend
    | Interest
    | Other
    | AdvisorFees

instance ToHttpApiData TransactionQueryType where
    toUrlPiece = \case
        All             -> "ALL"
        Trade_          -> "TRADE"
        BuyOnly         -> "BUY_ONLY"
        SellOnly        -> "SELL_ONLY"
        CashInOrCashOut -> "CASH_IN_OR_CASH_OUT"
        Checking        -> "CHECKING"
        Dividend        -> "DIVIDEND"
        Interest        -> "INTEREST"
        Other           -> "OTHER"
        AdvisorFees     -> "ADVISOR_FEES"

data AccountFields = Positions | Orders | PositionsAndOrders

instance ToHttpApiData AccountFields where
    toUrlPiece = \case
        Positions          -> "positions"
        Orders             -> "orders"
        PositionsAndOrders -> "positions,orders"

data Item = Item

-- APIs

type API =

-- Accounts and Trading
--
-- APIs to access Account Balances, Positions, Trade Info and place Trades

    -- Orders
    -- Saved Orders
    -- Accounts
    "accounts"
    :> Capture "accountId" AccountId
    :> QueryParam "fields" AccountFields -- one or both of 'positions,orders'
    :> Header "Authorization" AccessToken
    :> Get '[JSON] () -- Item

-- Authentication
--
-- oAuth API to retrieve the bearer token which can be used to access other APIs

-- Instruments
--
-- Search for instrument and fundamental data

-- Market Hours
--
-- Operating hours of markets

-- Movers
--
-- Retrieve mover information by index symbol, direction type and change

-- Option Chains
--
-- Get Option Chains for optionable symbols

-- Price History
--
-- Historical price data for charts

-- Quotes
--
-- Request real-time and delayed top level quote data

-- Transaction History
--
-- APIs to access transaction history on the account

    -- Get Transactions
    :<|> "accounts"
    :> Capture "accountId" AccountId
    :> "transactions"
    :> QueryParam "type" TransactionQueryType
    :> QueryParam "symbol" Symbol
    :> QueryParam "startDate" ISO8601Date
    :> QueryParam "endDate" ISO8601Date
    :> Header "Authorization" AccessToken
    :> Get '[JSON] TransactionHistory

-- User Info and Preferences
--
-- APIs to access user-authorized accounts and their preferences

-- Watchlist
--
-- APIs to perform CRUD operations on Account Watchlist

api :: Proxy API
api = Proxy

getAccounts
    :: AccountId
    -> Maybe AccountFields
    -> Maybe AccessToken
    -> ClientM () -- Item

getTransactions
    :: AccountId
    -> Maybe TransactionQueryType
    -> Maybe Symbol
    -> Maybe ISO8601Date
    -> Maybe ISO8601Date
    -> Maybe AccessToken
    -> ClientM TransactionHistory

getAccounts :<|> getTransactions = client api
