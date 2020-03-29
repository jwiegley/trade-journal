{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ThinkOrSwim.API where

import Data.Text as T
import Servant.API

baseUrl :: Text
baseUrl = "https://api.tdameritrade.com/v1/"

newtype Bearer = Bearer Text

instance Show Bearer where
    show (Bearer s) = T.unpack $ "Bearer " <> s

data AccountField = AFPositions | AFOrders

newtype AccountFields = AccountFields [AccountField]

daat

-- APIs

type Api =

-- Accounts and Trading
--
-- APIs to access Account Balances, Positions, Trade Info and place Trades

    -- Orders
    -- Saved Orders
    -- Accounts
    "accounts"
    :> Capture "accountId" Text
    :> QueryParam "fields" AccountFields -- one or both of 'positions,orders'
    :> Header "Authorization" Bearer
    :> Get '[JSON] Item

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

-- User Info and Preferences
--
-- APIs to access user-authorized accounts and their preferences

-- Watchlist
--
-- APIs to perform CRUD operations on Account Watchlist
