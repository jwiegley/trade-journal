{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Options where

import Control.Lens hiding (argument)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Options.Applicative as OA

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

data Command
  = Coinmetro !FilePath
  | ThinkOrSwim !FilePath
  | Journal !FilePath
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Command

data Options = Options
  { _verbose :: !Bool,
    _totals :: !Bool,
    _command :: !Command
  }
  deriving (Data, Show, Eq, Typeable, Generic)

makeLenses ''Options

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
    <*> hsubparser (coinmetroCommand <> thinkOrSwimCommand <> journalCommand)
  where
    coinmetroCommand :: Mod CommandFields Command
    coinmetroCommand =
      OA.command
        "coinmetro"
        (info coinmetroOptions (progDesc "Process Coinmetro CSV file"))
      where
        coinmetroOptions :: Parser Command
        coinmetroOptions =
          Coinmetro
            <$> strArgument (metavar "FILE" <> help "CSV file to read")

    thinkOrSwimCommand :: Mod CommandFields Command
    thinkOrSwimCommand =
      OA.command
        "thinkorswim"
        (info thinkOrSwimOptions (progDesc "Process ThinkOrSwim export file"))
      where
        thinkOrSwimOptions :: Parser Command
        thinkOrSwimOptions =
          ThinkOrSwim
            <$> strArgument (metavar "FILE" <> help "Export file to read")

    journalCommand :: Mod CommandFields Command
    journalCommand =
      OA.command
        "journal"
        (info journalOptions (progDesc "Process trade-journal file"))
      where
        journalOptions :: Parser Command
        journalOptions =
          Journal
            <$> strArgument (metavar "FILE" <> help "Journal file to read")

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
