{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Closings
import Control.Lens
import Control.Monad.Trans.Class
-- import Examples
-- import Gains
-- import GainsKeeper

import Data.Zipper
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import WashSaleRule

main :: IO ()
main =
  defaultMain $
    testGroup
      "journal"
      [ testProperty "zipper-unzipper" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $ xs @?= maybe xs unzipper (zipper even xs),
        testProperty "zipperM-unzipper" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $
            xs
              @?= maybe
                xs
                unzipper
                (runIdentity (zipperM (pure . even) xs)),
        --
        testProperty "projected" $ property do
          lift $
            inject @_ @'[Const Bool, Const Int] @() (Const True)
              @?= (projectedC # True),
        --
        testClosings,
        testWashSaleRule
        -- testGains,
        -- testGainsKeeper,
        -- testExamples
      ]
