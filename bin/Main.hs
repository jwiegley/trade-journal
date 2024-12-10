{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text.IO qualified as TL
import Ledger.Entry
import Ledger.Render
import Options qualified
import Trade.Journal.Parse
import Trade.Journal.Process
import Trade.Journal.Types

main :: IO ()
main = do
  opts <- Options.getOptions
  journal <- parseJournal (Options.journalFile opts)
  let ledger = processJournal newLedger journal
      ledger' = processLedger (const washSales) ledger

  print ledger'

  forM_ (M.toList (getLedger ledger')) $ \(sym, poss) -> do
    forM_ poss $ \pos -> do
      let xact = tradeTransaction "CSH" "EQTY" sym pos
      mapM_ TL.putStrLn $
        renderTransaction "Foo" xact
      TL.putStrLn ""
