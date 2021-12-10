{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative

version :: String
version = "0.0.1"

copyright :: String
copyright = "2020"

tradeJournalSummary :: String
tradeJournalSummary =
  "trade-journal "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data Options = Options
  { _verbose :: Bool,
    _totals :: Bool,
    _files :: [FilePath]
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Options

newOptions :: Options
newOptions =
  Options
    { _verbose = False,
      _totals = False,
      _files = []
    }

tradeJournalOpts :: Parser Options
tradeJournalOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> switch
      ( long "totals"
          <> help "Show calculated totals for entries"
      )
    <*> some (argument str (metavar "FILE"))

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
