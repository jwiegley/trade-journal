{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- {-# LANGUAGE TemplateHaskell #-}

module Journal.ThinkOrSwim.Process (thinkOrSwimToJournal) where

import Control.Arrow (left)
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Coerce
import qualified Data.Csv as Csv
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- import Journal.Amount
import Data.Time
import Data.Void (Void)
import Debug.Trace
import Journal.ThinkOrSwim.Parser
import Journal.ThinkOrSwim.Types
import Journal.Types
import Text.Megaparsec
import Text.Printf

entryTime :: Csv.NamedRecord -> UTCTime
entryTime record =
  case parseTimeM False defaultTimeLocale "%m/%d/%y %H:%M:%S" timeString of
    Nothing -> error $ "Could not parse date/time from " ++ show record
    Just t -> t
  where
    toStr = TL.unpack . TL.decodeUtf8 . BL.fromStrict

    splitString :: ByteString -> Char -> String
    splitString key ch =
      intercalate [ch] $
        map
          (printf "%02d" . (read :: String -> Int) . toStr)
          (B.split (fromIntegral (ord ch)) (record ^?! ix key))

    timeString =
      concat [splitString "DATE" '/', " ", splitString "TIME" ':']

entryParse :: Csv.NamedRecord -> Either (ParseErrorBundle Text Void) TOSEntry
entryParse record =
  parse
    parseEntry
    ""
    (TL.decodeUtf8 (BL.fromStrict (record ^?! ix "DESCRIPTION")))

entryToAction :: TOSEntry -> Either String Action
entryToAction = \case
  Bought _device TOSTrade {..} ->
    Right $
      Buy
        Lot
          { _amount = coerce tdQuantity,
            _symbol = TL.toStrict tdSymbol,
            _price = coerce tdPrice,
            _details = [], -- jww (2021-05-29): ???
            _computed = []
          }
  Sold _device TOSTrade {..} ->
    Right $
      Sell
        Lot
          { _amount = abs (coerce tdQuantity),
            _symbol = TL.toStrict tdSymbol,
            _price = coerce tdPrice,
            _details = [], -- jww (2021-05-29): ???
            _computed = []
          }
  x -> Left $ "Could not convert entry to action: " ++ show x

entry :: Csv.NamedRecord -> Either String (Timed Action)
entry record = do
  ent <- left show $ entryParse record
  act <- entryToAction ent
  pure $
    Timed
      { _time = entryTime record,
        _item = act
      }

thinkOrSwimToJournal :: ThinkOrSwim -> Journal
thinkOrSwimToJournal tos =
  Journal $
    flip concatMap (tos ^. xacts) $ \xact ->
      case entry xact of
        Left err -> trace err []
        Right x -> [x]
