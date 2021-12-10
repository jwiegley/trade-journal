{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Broker.ThinkOrSwim
import Control.Lens
import Control.Monad.Except
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Text.IO as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics hiding (to)
import qualified Journal.Closings as Closings
import Journal.Entry.Deposit
import Journal.Entry.Income
import Journal.Entry.Options
import Journal.Entry.Trade
import Journal.Parse
import Journal.Pipes
import Journal.Print
import qualified Options
import Taxes.USA.WashSaleRule

data Config = Config
  { splits :: Map Text [Amount 6],
    rounding :: Map Text (Amount 2)
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
            mapM_ TL.putStrLn (printEntries (thinkOrSwimEntries tos))
      else do
        putStrLn $ "Reading journal " ++ path
        entries <-
          parseEntries
            @_
            @'[Const Trade, Const Deposit, Const Income, Const Options]
            path
        parseProcessPrint
          (washSaleRule @_ @() . fst . Closings.closings Closings.FIFO)
          entries
          (mapM_ T.putStrLn)
