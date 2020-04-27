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
import           Data.Ledger as L
import           Data.Ledger.Render as L
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.IO as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           GHC.Generics hiding (to)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Servant.Client
import           ThinkOrSwim.API
import           ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Convert
import           ThinkOrSwim.Options as Opts
import           ThinkOrSwim.Types

data Holding = Holding
    { symbol :: Text
    , amount :: Amount 4
    , price  :: Amount 4
    , date   :: Day
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

    th <- case accessKey opts of
        Nothing -> case jsonData opts of
            Nothing   -> error "Neither --access-key nor --json-data provided"
            Just file -> readTransactions file
        Just key ->
            downloadTransactions (T.pack (Opts.account opts)) (T.pack key)
                =<< createManager

    let addLots xs = (xs & traverse.quantity %~ negate) ++ xs

    priceData <- case equity opts of
        Nothing -> pure newGainsKeeperState
        Just fp -> do
            eres <- Csv.decode Csv.NoHeader <$> BL.readFile fp
            case eres of
                Left err -> error $ "Failed to decode equity CSV: " ++ err
                Right holdings -> pure $ (`execState` newGainsKeeperState) $
                    forM_ holdings $ \h ->
                        let lot = lt L.Equity (Main.amount h)
                                     (Main.symbol h) (Main.price h)
                                     (Just (date h))
                        in openTransactions.at (Main.symbol h).non []
                             <>= addLots [lot]

    Prelude.putStrLn "; -*- ledger -*-\n"
    forM_ (convertOrders opts priceData th) $ \t -> do
        forM_ (renderTransaction t)
            T.putStrLn
        T.putStrLn ""
  where
    lt i q s p d = newCommodityLot @API.TransactionSubType
        & instrument   .~ i
        & kind         .~ TransferOfSecurityOrOptionIn
        & quantity     .~ q
        & L.symbol     .~ s
        & L.cost       ?~ abs (q * p)
        & washEligible .~ False
        & purchaseDate .~ d
        & lotId        .~ 0
        & refs         .~ [ Ref ExistingEquity 0 ]
        & L.price      ?~ p

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
