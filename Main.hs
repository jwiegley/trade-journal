{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Ledger as Ledger
import Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import System.Environment
import Text.Show.Pretty
import ThinkOrSwim.API
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Convert

createManager :: IO Manager
createManager =
    newTlsManagerWith $
        managerSetProxy (proxyEnvironment Nothing)
            tlsManagerSettings {
                -- jww (2020-03-29): servant by default sets
                -- "Accept: application/json;charset=utf-8,application/json"
                -- However, sending this request results in error code 500.
                managerModifyRequest = \req -> return $
                    req { requestHeaders = ("Accept", "application/json") :
                            Prelude.filter (("Accept" /=) . fst)
                                           (requestHeaders req) }
                }

downloadTransactions :: Text -> Text -> Manager -> IO [API.Transaction]
downloadTransactions actId accessToken mgr = do
    let call = getTransactions
            (AccountId actId)
            Nothing
            Nothing
            Nothing
            Nothing
            (Just (AccessToken accessToken))
    res <- runClientM call (env mgr)
    case res of
        Left err -> error $ "Error: " ++ show err
        Right xs -> return xs
  where
    env = flip mkClientEnv $ BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost   = "api.tdameritrade.com"
        , baseUrlPort   = 443
        , baseUrlPath   = "/v1"
        }

readTransactions :: IO [API.Transaction]
readTransactions = do
    eres <- eitherDecode
        <$> BL.readFile "/Users/johnw/Documents/accounts/broker/hist.json"
    case eres of
        Left err  -> error err
        Right res -> pure res

main :: IO ()
main = do
    act:token:_ <- Prelude.map T.pack <$> getArgs
    mgr         <- createManager
    xs:_        <- cleanupTransactions
        <$> (readTransactions <|> downloadTransactions act token mgr)
    pPrint xs
    pPrint (convertTransaction xs)
    let [xs'] = finalizeTransactions [] [convertTransaction xs]
    pPrint (renderTransaction xs')
