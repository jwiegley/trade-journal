{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ThinkOrSwim.Options where

import Control.Applicative
import Control.Lens
import Data.Data (Data)
import Data.Typeable (Typeable)
import Options.Applicative

version :: String
version = "0.0.1"

copyright :: String
copyright = "2020"

thinkorswimSummary :: String
thinkorswimSummary =
    "thinkorswim " ++ version ++ ", (C) " ++ copyright ++ " John Wiegley"

data Options = Options
    { _dryRun          :: Bool
    , _verbose         :: Bool
    , _startDate       :: Maybe String
    , _endDate         :: Maybe String
    , _account         :: String
    , _accessKey       :: Maybe String
    , _jsonData        :: Maybe FilePath
    , _equity          :: Maybe FilePath
    , _capitalGains    :: Bool
    , _washSaleRule    :: Bool
    , _traceSymbol     :: Maybe String
    , _traceUnderlying :: Maybe String
    , _traceId         :: Maybe String
    , _traceAll        :: Bool
    }
    deriving (Data, Typeable, Show, Eq)

makeLenses ''Options

newOptions :: Options
newOptions = Options
    { _dryRun          = False
    , _verbose         = False
    , _startDate       = Nothing
    , _endDate         = Nothing
    , _account         = ""
    , _accessKey       = Nothing
    , _jsonData        = Nothing
    , _equity          = Nothing
    , _capitalGains    = False
    , _washSaleRule    = False
    , _traceSymbol     = Nothing
    , _traceUnderlying = Nothing
    , _traceId         = Nothing
    , _traceAll        = False
    }

thinkorswimOpts :: Parser Options
thinkorswimOpts = Options
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
    <*> optional (strOption
        (   long "equity"
         <> help "CSV file containing details on existing equity"))
    <*> switch
        (   long "capital-gains"
         <> help "Calculate capital gains")
    <*> switch
        (   long "wash-sale-rule"
         <> help "Calculate the wash sale rule on capital gains")
    <*> optional (strOption
        (   long "trace-symbol"
         <> help "transaction symbol to trace"))
    <*> optional (strOption
        (   long "trace-underlying"
         <> help "transaction underlying to trace"))
    <*> optional (strOption
        (   long "trace-xid"
         <> help "transaction id to trace"))
    <*> switch
        (   long "trace"
         <> help "trace all transactions")

optionsDefinition :: ParserInfo Options
optionsDefinition = info
    (helper <*> thinkorswimOpts)
    (fullDesc <> progDesc "" <> header thinkorswimSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
