{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Broker.ThinkOrSwim
import Control.Lens
import Control.Monad.Except
import Data.Aeson hiding ((.=))
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import qualified Journal.Closings as Closings
import Journal.Parse
import Journal.Pipes
import Journal.Print
import Options
import Taxes.USA.WashSaleRule
import Text.Megaparsec (many)

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
            putStrLn . T.unpack . T.concat $
              printActions
                (either id id . printWashing)
                (thinkOrSwimActions tos)
      else do
        putStrLn $ "Reading journal " ++ path
        entries <- parseActionsAndEvents (many parseWashing) path
        parseProcessPrint
          Closings.FIFO
          washSaleRule
          (TL.concat . map (either id id . printWashing))
          entries
          (putStrLn . T.unpack . T.concat)
