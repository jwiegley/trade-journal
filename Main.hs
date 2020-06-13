{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Amount
import Data.ByteString.Lazy as BL
import Data.Ledger.Render as L
import Data.Map (Map, assocs)
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lens
import Data.Time
import GHC.Generics hiding (to)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Servant.Client
import ThinkOrSwim.API
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
         hiding (symbol, amount, price)
import ThinkOrSwim.Convert
import ThinkOrSwim.Event hiding (symbol)
import ThinkOrSwim.Options as Opts

data Holding = Holding
    { amount :: Amount 6
    , price  :: Amount 6
    , date   :: Day
    }
    deriving (Generic, Show, FromJSON)

data Config = Config
    { holdings :: Map Text [Holding]
    , splits   :: Map Text [Amount 6]
    , rounding :: Map Text (Amount 2)
    }
    deriving (Generic, Show, FromJSON)

newConfig :: Config
newConfig = Config
    { holdings = mempty
    , splits   = mempty
    , rounding   = mempty
    }

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

    config <- case opts^.equity of
        Nothing -> pure newConfig
        Just fp -> do
            eres <- eitherDecode <$> BL.readFile fp
            case eres of
                Left err  -> error $ "Failed to decode config: " ++ err
                Right cfg -> pure cfg

    let ts = flip evalState newGainsKeeperState $ do
            forM_ (assocs (holdings config)) $ \(sym, hs) ->
                forM_ hs $ \h ->
                    positionEvents.at sym.non []
                        <>= [ EquityCost
                                (amount h)
                                (- (price h * amount h))
                                (UTCTime (date h) 0) ]
            roundingEntries .= rounding config
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
