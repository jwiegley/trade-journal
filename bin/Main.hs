{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Control.Lens hiding (Context)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as TL
import GHC.Generics hiding (to)
import Options qualified
import Trade.Closings qualified as Closings
import Trade.Journal.Print
import Trade.Journal.Types
import Trade.Process
import Trade.Provider.Coinmetro qualified as Coinmetro
import Trade.Provider.ThinkOrSwim qualified as ThinkOrSwim

data Config = Config
  { splits :: !(Map Text [Amount 6]),
    rounding :: !(Map Text (Amount 2))
  }
  deriving (Generic, Show)

newConfig :: Config
newConfig =
  Config
    { splits = mempty,
      rounding = mempty
    }

main :: IO ()
main = do
  opts <- Options.getOptions
  case opts ^. Options.command of
    Options.ThinkOrSwim path -> do
      putStrLn $ "Reading ThinkOrSwim CSV export " ++ path
      etos <- ThinkOrSwim.readCsv path
      case etos of
        Left err -> error $ "Error " ++ show err
        Right tos ->
          mapM_
            TL.putStrLn
            ( printEntries
                ( ThinkOrSwim.thinkOrSwimEntries
                    Context
                      { _account = "",
                        _currency = ""
                      }
                    tos
                )
            )
    Options.Coinmetro path -> do
      putStrLn $ "Reading Coinmetro CSV export " ++ path
      ecm <- Coinmetro.readCsv path
      case ecm of
        Left err -> error $ "Error " ++ show err
        Right cm ->
          mapM_
            TL.putStrLn
            ( printEntries
                ( Coinmetro.coinmetroEntries
                    Context
                      { _account = "",
                        _currency = ""
                      }
                    cm
                )
            )
    Options.Journal path -> do
      putStrLn $ "Reading journal " ++ path
      entries <- parseJournalEntries path
      let (processedEntries, _openPositions) =
            processJournal
              ProcessingEnvironment
                { lotCalculationMethod = Closings.FIFO,
                  washSales = True
                }
              entries
      mapM_ TL.putStrLn $ printEntries processedEntries
