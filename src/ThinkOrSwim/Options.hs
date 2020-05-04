{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module ThinkOrSwim.Options where

import Control.Applicative
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
    { dryRun          :: Bool
    , verbose         :: Bool
    , startDate       :: Maybe String
    , endDate         :: Maybe String
    , account         :: String
    , accessKey       :: Maybe String
    , jsonData        :: Maybe FilePath
    , equity          :: Maybe FilePath
    , capitalGains    :: Bool
    , washSaleRule    :: Bool
    , traceSymbol     :: Maybe String
    , traceUnderlying :: Maybe String
    , traceId         :: Maybe String
    , traceAll        :: Bool
    }
    deriving (Data, Typeable, Show, Eq)

newOptions :: Options
newOptions = Options
    { dryRun          = False
    , verbose         = False
    , startDate       = Nothing
    , endDate         = Nothing
    , account         = ""
    , accessKey       = Nothing
    , jsonData        = Nothing
    , equity          = Nothing
    , capitalGains    = False
    , washSaleRule    = False
    , traceSymbol     = Nothing
    , traceUnderlying = Nothing
    , traceId         = Nothing
    , traceAll        = False
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
