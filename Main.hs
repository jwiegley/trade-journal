{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (forM_)
import           Data.Aeson
import           Data.ByteString.Lazy as BL
import           Data.Data (Data)
import           Data.Ledger as Ledger
import qualified Data.Map as M
import           Data.Text as T
import           Data.Text.IO as T
import           Data.Time
import           Data.Typeable (Typeable)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           Servant.Client
import           ThinkOrSwim.API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Convert
import           ThinkOrSwim.Gains

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

    let priceData = M.empty
            & at "ZM"   ?~
                [ lot Stock  30 "ZM" 106.68   "2019-06-24"
                , lot Stock 140 "ZM"  99.7792 "2019-06-24"
                , lot Stock  10 "ZM"  89.785  "2019-06-24"
                , lot Stock 170 "ZM"  85.8415 "2019-06-25" ]
            & at "CRWD" ?~
                [ lot Stock 140 "CRWD" 73.7914 "2019-06-20"
                , lot Stock 140 "CRWD" 69.683  "2019-06-21" ]
            & at "WORK" ?~
                [ lot Stock 250 "WORK" 38.97284 "2019-06-20" ]

    -- Prelude.putStrLn "--- Data converted to Ledger format ---"
    forM_ (convertTransactions priceData th) $ \t -> do
        forM_ (renderTransaction t)
            T.putStrLn
        T.putStrLn ""
  where
    lot i q s p d = CommodityLot
        { _instrument    = i
        , _quantity      = q
        , Ledger._symbol = s
        , Ledger._cost   = Just (q * p)
        , _purchaseDate  = Just (parseTimeOrError False defaultTimeLocale
                                                  "%Y-%m-%d" d)
        , _refs          = []
        , Ledger._price  = Just p
        }

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
