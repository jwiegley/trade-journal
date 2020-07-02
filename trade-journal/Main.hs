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
import Data.Foldable
import Data.Map (Map)
import Data.Text (Text)
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

main :: IO ()
main = do
  path : _ <- getArgs
  journal <- TL.decodeUtf8 <$> BL.readFile path
  actions <- case parse parseJournal path journal of
    Left e -> fail $ errorBundlePretty e
    Right x -> pure x
  let (_, res) = processActions actions
  forM_ res $ \lot ->
    TL.putStrLn $ printLot lot
