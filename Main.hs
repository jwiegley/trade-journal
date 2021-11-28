{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Amount
import Control.Lens
import Control.Monad.Except
import Data.Aeson hiding ((.=))
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Generics hiding (to)
import Journal.Parse
import Journal.Pipes
import Journal.Print
import Journal.ThinkOrSwim
import Options
import qualified Pipes.Prelude.Text as P

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
  opts <- getOptions
  forM_ (opts ^. files) $ \path ->
    if ".csv" `isSuffixOf` path
      then do
        putStrLn $ "Reading ThinkOrSwim CSV export " ++ path
        etos <- readCsv path
        case etos of
          Left err -> error $ "Error " ++ show err
          Right tos ->
            handleActions
              (thinkOrSwimActions tos)
              printActions
              P.stdoutLn
      else do
        putStrLn $ "Reading journal " ++ path
        parseProcessPrint (parseActionsAndEvents path) P.stdoutLn
