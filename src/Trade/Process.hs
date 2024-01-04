{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Trade.Process where

import Control.Lens
import Data.Data
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text.Lazy (Text)
import GHC.Generics hiding (to)
import Text.Show.Pretty hiding (Time)
import Trade.Closings (Calculation, PositionEvent, closings)
import Trade.Journal.Entry (AsEntry (..), Entry)
import Trade.Journal.Parse (parseEntries)
import Trade.Journal.Print (Printable (..), printEntries)
import Trade.Journal.Types (Annotated, item)
import Trade.Journal.Types.Lot (HasLot (..))
import Trade.Journal.Utils (meld)
import Trade.Taxes.USA.WashSaleRule (Washing, washSaleRule)

data ProcessedEntry
  = AnEntry !Entry
  | APositionEvent !PositionEvent
  | AWashing !Washing
  deriving (Show, Eq, PrettyVal, Generic, Data)

makeClassyPrisms ''ProcessedEntry

instance Printable ProcessedEntry where
  printItem (AnEntry entry) = printItem entry
  printItem (APositionEvent event) = printItem event
  printItem (AWashing washing) = printItem washing

instance HasLot ProcessedEntry where
  _Lot f (AnEntry s) = AnEntry <$> (s & _Lot %%~ f)
  _Lot f (APositionEvent s) = APositionEvent <$> (s & _Lot %%~ f)
  _Lot f (AWashing s) = AWashing <$> (s & _Lot %%~ f)

parseJournalEntries :: FilePath -> IO [Annotated Entry]
parseJournalEntries = parseEntries

printJournalEntries :: [Annotated Entry] -> [Text]
printJournalEntries = printEntries

processPositionEvents ::
  Calculation (Annotated PositionEvent) ->
  [Annotated Entry] ->
  ( [[Annotated PositionEvent]],
    Map T.Text (IntMap (Annotated PositionEvent))
  )
processPositionEvents calc = closings calc _TradeEntry

washPositionEvents ::
  [Annotated PositionEvent] ->
  [Annotated (Either PositionEvent Washing)]
washPositionEvents = washSaleRule id

processJournal ::
  Calculation (Annotated PositionEvent) ->
  Bool ->
  [Annotated Entry] ->
  ( [Annotated ProcessedEntry],
    Map T.Text (IntMap (Annotated PositionEvent))
  )
processJournal calc washSales (entries :: [Annotated Entry]) =
  let (events :: [[Annotated PositionEvent]], positions) =
        processPositionEvents calc entries
      processedEvents :: [Annotated ProcessedEntry] =
        map combine (meld entries events)
   in ( if washSales
          then
            map
              (fmap (either id AWashing))
              (washSaleRule _APositionEvent processedEvents)
          else processedEvents,
        positions
      )
  where
    combine (Left ent) = fmap AnEntry ent
    combine (Right pe) = fmap APositionEvent pe

entriesOnly :: [Annotated ProcessedEntry] -> [Annotated Entry]
entriesOnly [] = []
entriesOnly (x : xs) = case x ^? item . _AnEntry of
  Just entry -> (entry <$ x) : entriesOnly xs
  Nothing -> entriesOnly xs

positionsOnly :: [Annotated ProcessedEntry] -> [Annotated PositionEvent]
positionsOnly [] = []
positionsOnly (x : xs) = case x ^? item . _APositionEvent of
  Just entry -> (entry <$ x) : positionsOnly xs
  Nothing -> positionsOnly xs
