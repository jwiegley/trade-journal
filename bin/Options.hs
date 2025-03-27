module Options where

import Options.Applicative as OA

version :: String
version = "0.0.1"

copyright :: String
copyright = "2020-2025"

tradeJournalSummary :: String
tradeJournalSummary =
  "trade-journal "
    ++ version
    ++ ", (C) "
    ++ copyright
    ++ " John Wiegley"

data Options = Options
  { verbose :: !Bool,
    journalFile :: !(Maybe FilePath),
    broker :: !(Maybe String),
    account :: !(Maybe String),
    command :: !String,
    arguments :: ![String]
  }
  deriving (Show, Eq)

tradeJournalOpts :: Parser Options
tradeJournalOpts =
  Options
    <$> switch
      ( short 'v'
          <> long "verbose"
          <> help "Report progress verbosely"
      )
    <*> optional
      ( strOption
          ( short 'f'
              <> long "file"
              <> help "Path to journal file"
          )
      )
    <*> optional
      ( strOption
          ( short 'b'
              <> long "broker"
              <> help "Name of brokerage account"
          )
      )
    <*> optional
      ( strOption
          ( short 'a'
              <> long "account"
              <> help "Name of account at the brokerage"
          )
      )
    <*> strArgument (help "Command to execute")
    <*> many
      ( argument
          (eitherReader Right)
          (help "Command arguments" <> metavar "ARGS")
      )

optionsDefinition :: ParserInfo Options
optionsDefinition =
  info
    (helper <*> tradeJournalOpts)
    (fullDesc <> progDesc "" <> header tradeJournalSummary)

getOptions :: IO Options
getOptions = execParser optionsDefinition
