{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Map.Strict qualified as M
-- import Text.Show.Pretty

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
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
  -- pPrint journal

  ledger <- (\f -> foldM f newLedger (getJournal journal)) $
    \(Ledger ledger) (sym, trade@(Trade lot _ _)) ->
      fmap Ledger $ (\f -> M.alterF f sym ledger) $ \mposs -> do
        -- putStrLn "----------------------------------------"
        -- pPrint mposs
        -- pPrint lot
        let changes = applyLot lot (concat mposs)
        -- pPrint changes
        let xact =
              transactionFromChanges
                ""
                (T.pack (fromMaybe "" (Options.account opts)))
                sym
                trade
                changes
        -- pPrint xact
        mapM_ T.putStrLn $
          renderTransaction (T.pack (Options.broker opts)) xact
        T.putStrLn ""
        pure $ Just $ changedPositions changes

  -- pPrint ledger
  let _ledger' = processLedger (const washSales) ledger
  -- pPrint ledger'

  pure ()

-- forM_ (M.toList (getLedger ledger')) $ \(sym, poss) -> do
--   forM_ poss $ \pos -> do
--     let xact = tradeTransaction "CSH" "EQTY" sym pos
--     mapM_ TL.putStrLn $
--       renderTransaction "Foo" xact
--     TL.putStrLn ""

-- jww (2025-01-06): Turn final set of open positions into list of account
-- balance assertions.
