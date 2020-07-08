{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics hiding (to)
import Journal.Amount
import Journal.Model
import Journal.Parse
import System.Environment
import Text.Megaparsec (parse)
import Text.Megaparsec.Error

data Config = Config
  { splits :: Map Text [Amount 6],
    rounding :: Map Text (Amount 2)
  }
  deriving (Generic, Show, FromJSON)

newConfig :: Config
newConfig =
  Config
    { splits = mempty,
      rounding = mempty
    }

parseProcessPrint :: MonadFail m => FilePath -> TL.Text -> m TL.Text
parseProcessPrint path journal = do
  actions <- case parse parseJournal path journal of
    Left e -> fail $ errorBundlePretty e
    Right res -> pure res
  case processJournal actions of
    Left err ->
      error $ "Error processing journal " ++ path ++ ": " ++ show err
    Right j -> pure $ printJournal j

main :: IO ()
main = do
  path : _ <- getArgs
  putStrLn $ "Reading journal " ++ path
  journal <- TL.decodeUtf8 <$> BL.readFile path
  TL.putStrLn =<< parseProcessPrint path journal
