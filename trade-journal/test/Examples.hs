{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples (testExamples) where

import Data.List (intersperse)
import Data.String.Here.Interpolated
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Journal.Model
import Journal.Parse
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

testExamples :: TestTree
testExamples =
  testGroup
    "examples"
    [ baseline
    ]

baseline :: TestTree
baseline =
  testGroup
    "baseline"
    [ testCase "journal-simple-buy" journalSimpleBuy
    ]

journalSimpleBuy :: Assertion
journalSimpleBuy = ii @--> oo
  where
    ii =
      [i|2020-07-02 buy 100 AAPL 260.00
         2020-07-03 sell 100 AAPL 300.00
        |]
    oo =
      [i|2020-07-02 00:00:00 100.000000 AAPL 260.000000 open
         2020-07-03 00:00:00 -100.000000 AAPL 300.000000 close gain 40.000000
        |]

(@-->) :: Text -> Text -> Assertion
x @--> y = do
  y' <- parseProcessPrint "" x
  trimLines y @?= trimLines y'
  where
    trimLines =
      TL.concat
        . intersperse "\n"
        . map TL.strip
        . TL.splitOn "\n"
        . TL.strip
    parseProcessPrint :: MonadFail m => FilePath -> Text -> m Text
    parseProcessPrint path journal = do
      actions <- case parse parseJournal path journal of
        Left e -> fail $ errorBundlePretty e
        Right res -> pure res
      let (_, res) = processActions actions
      pure $ TL.concat $ intersperse "\n" $ map printLot res
