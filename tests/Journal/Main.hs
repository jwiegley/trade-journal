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

import Data.Sum
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Sum.Lens
import Data.Zippered
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import WashSaleRule

main :: IO ()
main =
  defaultMain $
    testGroup
      "journal"
      [ testProperty "zippered-unzippered" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $ xs @?= maybe xs unzippered (zippered even xs),
        testProperty "zipperedM-unzippered" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $
            xs
              @?= maybe
                xs
                unzippered
                (runIdentity (zipperedM (pure . even) xs)),
        --
        testProperty "reverseZippered-reverseUnzippered" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $ xs @?= maybe xs reverseUnzippered (reverseZippered even xs),
        testProperty "reverseZipperedM-reverseUnzippered" $ property do
          xs <-
            forAll $
              Gen.list
                (Range.linear 0 100)
                (Gen.int (Range.linear 0 100))
          lift $
            xs
              @?= maybe
                xs
                reverseUnzippered
                (runIdentity (reverseZipperedM (pure . even) xs)),
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
