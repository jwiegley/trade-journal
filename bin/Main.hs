{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Control.Lens hiding (Context)
import Data.Foldable (forM_)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics hiding (to)
import qualified Trade.Closings as Closings
import qualified Options
import Trade.Process
import Trade.Journal.Print
import Trade.Journal.Types
import Trade.Provider.ThinkOrSwim hiding (_account)

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
  forM_ (opts ^. Options.files) $ \path ->
    if ".csv" `isSuffixOf` path
      then do
        putStrLn $ "Reading ThinkOrSwim CSV export " ++ path
        etos <- readCsv path
        case etos of
          Left err -> error $ "Error " ++ show err
          Right tos ->
            mapM_
              TL.putStrLn
              ( printEntries
                  ( thinkOrSwimEntries
                      Context
                        { _account = "",
                          _currency = ""
                        }
                      tos
                  )
              )
      else do
        putStrLn $ "Reading journal " ++ path
        entries <- parseJournalEntries path
        let (processedEntries, _openPositions) =
                  processJournal Closings.FIFO True entries
        mapM_ TL.putStrLn $ printEntries processedEntries
