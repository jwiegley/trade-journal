{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Class
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import TestClosings
import Trade.Data.Zipper

-- import Examples
-- import Gains
-- import GainsKeeper
-- import TestWashSaleRule

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
        testClosings
        -- testWashSaleRule
        -- testGains,
        -- testGainsKeeper,
        -- testExamples
      ]
