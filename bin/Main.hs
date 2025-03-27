{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Ledger.Entry
import Ledger.Render
import Options qualified
import Text.Show.Pretty
import Trade.Journal.Parse
import Trade.Journal.Process
import Trade.Journal.Types
import Trade.Provider.Coinmetro.Parser qualified as Coinmetro
import Trade.Provider.Coinmetro.Process qualified as Coinmetro

main :: IO ()
main = do
  opts <- Options.getOptions

  case Options.command opts of
    "coinmetro" ->
      case Options.arguments opts of
        file : _ -> do
          eres <- Coinmetro.readCsv file
          case eres of
            Left err ->
              error $ "Error reading Coinmetro file: " ++ TL.unpack err
            Right journal -> do
              pPrintList journal
              let entries = Coinmetro.coinmetroEntries journal
              pPrintList entries
        _ -> error "Usage: trade coinmetro FILE"
    _ -> case (,)
      <$> Options.journalFile opts
      <*> Options.broker opts of
      Nothing ->
        error "Usage: trade -f FILE -b NAME"
      Just (journal, broker) -> do
        j <- parseJournal journal
        -- pPrint journal

        ledger <- (\f -> foldM f newLedger (getJournal j)) $
          \(Ledger ledger) (sym, trade@(Trade lot _)) ->
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
                renderTransaction (T.pack broker) xact
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
