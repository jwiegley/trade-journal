{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ThinkOrSwim.API.TransactionHistory.GetTransactions where

import Servant.API
import Data.Text as T

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

newtype BearerToken = BearerToken Text

instance Show BearerToken where
    show (BearerToken s) = T.unpack $ "Bearer " <> s

type GetTransactions =
    "accounts"
    :> Capture "accountId" Text
    :> "transactions"
    :> QueryParam "type" TransactionQueryType
    :> QueryParam "symbol" Symbol
    :> QueryParam "startDate" ISO8601Date
    :> QueryParam "endDate" ISO8601Date
    :> Header "Authorization" BearerToken
    :> Get '[JSON] [Transaction]
