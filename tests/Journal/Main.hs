{-# LANGUAGE BlockArguments #-}

module Main where

import Closings
import Control.Monad.Trans.Class
-- import Examples
-- import Gains
-- import GainsKeeper

import Data.Functor.Identity
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Zippered
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
        testClosings,
        testWashSaleRule
        -- testGains,
        -- testGainsKeeper,
        -- testExamples
      ]
