{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Journal.ThinkOrSwim where

import Control.Lens
import Control.Monad.State
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- import Data.Time
import Data.Vector (toList)
-- import Journal.Amount
import System.IO

data ThinkOrSwim = ThinkOrSwim
  { _account :: Text,
    _xacts :: [Csv.NamedRecord],
    _futures :: [Csv.NamedRecord],
    _forex :: [Csv.NamedRecord],
    _orders :: [Csv.NamedRecord],
    _trades :: [Csv.NamedRecord],
    _equities :: [Csv.NamedRecord],
    _options :: [Csv.NamedRecord],
    _pandl :: [Csv.NamedRecord],
    _byOrderId ::
      Map
        B.ByteString
        ([Csv.NamedRecord], [Csv.NamedRecord], [Csv.NamedRecord])
  }

makeLenses ''ThinkOrSwim

readSection :: Handle -> IO [Csv.NamedRecord]
readSection h =
  fmap (toList . snd)
    . check
    . Csv.decodeByNameWithP pure Csv.defaultDecodeOptions
    =<< readUntilBlank
  where
    readUntilBlank = do
      line <- BL.fromStrict <$> B.hGetLine h
      if BL.null line
        then pure line
        else do
          rest <- readUntilBlank
          pure $ line <> "\n" <> rest
    check :: MonadFail m => Either String a -> m a
    check (Left err) = fail $ "Error: " ++ err
    check (Right res) = pure res

readCsv :: FilePath -> IO ThinkOrSwim
readCsv path = withFile path ReadMode $ \h -> do
  "\65279Account" : "Statement" : "for" : _account : _ <-
    TL.words . TL.decodeUtf8 <$> readLine h
  "" <- readLine h
  "Cash Balance" <- readLine h
  _xacts <- readSection h
  "Futures Statements" <- readLine h
  _futures <- readSection h
  "Forex Statements" <- readLine h
  _forex <- readSection h
  " " <- readLine h
  "\"Total" : "Cash" : _ <-
    TL.words . TL.decodeUtf8 <$> readLine h
  "" <- readLine h
  "" <- readLine h
  "Account Order History" <- readLine h
  _orders <- readSection h
  "Account Trade History" <- readLine h
  _trades <- readSection h
  "Equities" <- readLine h
  _equities <- readSection h
  "Options" <- readLine h
  _options <- readSection h
  "Profits and Losses" <- readLine h
  _pandl <- readSection h
  let _byOrderId = flip execState mempty $ do
        forM_ _xacts $ \xact -> do
          entry (xact ^?! ix "REF #") . _1 <>= [xact]
        scanOrders _2 _orders
        scanOrders _3 _trades
  pure ThinkOrSwim {..}
  where
    entry oid = at oid . non ([], [], [])
    scanOrders l = flip foldM_ Nothing $ \lx x -> do
      let oid = x ^?! ix "Order ID"
      entry
        ( case lx of
            Nothing -> oid
            Just o
              | B.null oid -> o
              | otherwise -> oid
        )
        . l
        <>= [x]
      pure $
        if null x
          then lx
          else Just oid
    readLine :: Handle -> IO ByteString
    readLine h = do
      line <- BL.fromStrict <$> B.hGetLine h
      -- print line
      pure line
