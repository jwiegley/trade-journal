{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (forM_)
import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Data (Data)
import Data.Ledger as Ledger
import Data.Text as T
import Data.Text.IO as T
import Data.Typeable (Typeable)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Servant.Client
import ThinkOrSwim.API
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Convert

version :: String
version = "0.0.1"

copyright :: String
copyright = "2020"

thinkorswimSummary :: String
thinkorswimSummary =
    "thinkorswim " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data Options = Options
    { dryRun    :: Bool
    , verbose   :: Bool
    , startDate :: Maybe String
    , endDate   :: Maybe String
    , account   :: String
    , accessKey :: Maybe String
    , jsonData  :: Maybe FilePath
    }
    deriving (Data, Typeable, Show, Eq)

thinkorswimOpts :: Parser Main.Options
thinkorswimOpts = Main.Options
    <$> switch
        (   short 'n'
         <> long "dry-run"
         <> help "Don't take any actions")
    <*> switch
        (   short 'v'
         <> long "verbose"
         <> help "Report progress verbosely")
    <*> optional (strOption
        (   long "start-date"
         <> help "download transactions starting from this date, YYYY-mm-dd format"))
    <*> optional (strOption
        (   long "end-date"
         <> help "download transactions up to this date, YYYY-mm-dd format"))
    <*> strOption
        (   long "account"
         <> help "account number to download for")
    <*> optional (strOption
        (   long "access-key"
         <> help "Use a specific ssh command"))
    <*> optional (strOption
        (   long "json-data"
         <> help "JSON file containing already downloaded data"))

optionsDefinition :: ParserInfo Main.Options
optionsDefinition = info
    (helper <*> thinkorswimOpts)
    (fullDesc <> progDesc "" <> header thinkorswimSummary)

getOptions :: IO Main.Options
getOptions = execParser optionsDefinition

main :: IO ()
main = do
    opts <- getOptions

    th <- case accessKey opts of
        Nothing -> case jsonData opts of
            Nothing   -> error "Neither --access-key nor --json-data provided"
            Just file -> readTransactions file
        Just key ->
            downloadTransactions (T.pack (Main.account opts)) (T.pack key)
                =<< createManager

    -- Prelude.putStrLn "--- Data read from JSON ---"
    -- pPrint th

    -- Prelude.putStrLn "--- Data converted to Ledger format ---"
    forM_ (convertTransactions th) $ \t -> do
        forM_ (renderTransaction t)
            T.putStrLn
        T.putStrLn ""

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

downloadTransactions :: Text -> Text -> Manager -> IO TransactionHistory
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
        Right th -> return th
  where
    env = flip mkClientEnv $ BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost   = "api.tdameritrade.com"
        , baseUrlPort   = 443
        , baseUrlPath   = "/v1"
        }

readTransactions :: FilePath -> IO TransactionHistory
readTransactions fp = either fail pure =<< eitherDecode <$> BL.readFile fp
