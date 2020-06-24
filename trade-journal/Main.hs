{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Aeson hiding ((.=))
import Data.Map (Map)
import Data.Text as T
import GHC.Generics hiding (to)
import Journal.Amount

data Config
  = Config
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
main = pure ()
