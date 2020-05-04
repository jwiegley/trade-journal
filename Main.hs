{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Aeson
import           Data.Amount
import           Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Foldable
import           Data.Ledger.Render as L
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.IO as T
import           Data.Text.Lens
import           Data.Time
import           Data.Time.Format.ISO8601
import           GHC.Generics hiding (to)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Servant.Client
import           ThinkOrSwim.API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Convert
import           ThinkOrSwim.Event
import           ThinkOrSwim.Options as Opts

data Holding = Holding
    { holdSymbol :: Text
    , holdAmount :: Amount 6
    , holdPrice  :: Amount 6
    , holdDate   :: Day
    }
    deriving (Generic, Show)

instance Csv.FromField Day where
    parseField = iso8601ParseM . T.unpack . T.decodeUtf8

instance Csv.ToField Day where
    toField = T.encodeUtf8 . T.pack . iso8601Show

instance Csv.FromRecord Holding
instance Csv.ToRecord Holding

main :: IO ()
main = do
    opts <- getOptions

    th <- case opts^.accessKey of
        Nothing -> case opts^.jsonData of
            Nothing   -> error "Neither --access-key nor --json-data provided"
            Just file -> readTransactions file
        Just key ->
            downloadTransactions (opts^.Opts.account.packed) (T.pack key)
                =<< createManager

    priceData <- case opts^.equity of
        Nothing -> pure []
        Just fp -> do
            eres <- Csv.decode Csv.NoHeader <$> BL.readFile fp
            case eres of
                Left err -> error $ "Failed to decode equity CSV: " ++ err
                Right holdings -> pure $ toList holdings

    let ts = flip evalState newGainsKeeperState $ do
            forM_ priceData $ \h ->
                positionEvents.at (holdSymbol h).non []
                    <>= [ EstablishEquityCost
                            (holdAmount h)
                            (- (holdPrice h * holdAmount h))
                            (UTCTime (holdDate h) 0) ]
            convertOrders opts th

    Prelude.putStrLn "; -*- ledger -*-\n"
    forM_ ts $ \t -> do
        forM_ (renderTransaction t) $
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
