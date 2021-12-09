{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Broker.ThinkOrSwim
import Control.Lens
import Control.Monad.Except
import Data.Aeson hiding ((.=))
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Text.IO as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics hiding (to)
import qualified Journal.Closings as Closings
import Journal.Entry
import Journal.Entry.Trade
import Journal.Parse
import Journal.Pipes
import Journal.Print
import Journal.SumLens
import Options
import Taxes.USA.WashSaleRule

data Config = Config
  { splits :: Map Text [Amount 6],
    rounding :: Map Text (Amount 2),
    modes :: Map Text Closings.Calculation
  }
  deriving (Generic, Show, FromJSON)

newConfig :: Config
newConfig =
  Config
    { splits = mempty,
      rounding = mempty,
      modes = mempty
    }

main :: IO ()
main = do
  opts <- getOptions
  forM_ (opts ^. files) $ \path ->
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
        entries <- parseEntries path
        parseProcessPrint
          (washSaleRule @_ @() . fst . Closings.closings Closings.FIFO)
          (map (fmap (projectedC @'[Const Trade, Const Entry] #)) entries)
          (mapM_ T.putStrLn)
