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
    [ testCase "journal-buy-sell-profit" journalBuySellProfit,
      testCase "journal-buy-sell-loss-buy" journalBuySellLossBuy
    ]

journalBuySellProfit :: Assertion
journalBuySellProfit = ii @--> oo
  where
    ii =
      [i|2020-07-02 buy 100 AAPL 260.00
         2020-07-03 sell 100 AAPL 300.00 fees 0.20
        |]
    oo =
      [i|2020-07-02 00:00:00 buy 100 AAPL 260.0000 open
         2020-07-03 00:00:00 sell 100 AAPL 300.0000 fees 0.20 close gain 40.000000
        |]

journalBuySellLossBuy :: Assertion
journalBuySellLossBuy = ii @--> oo
  where
    ii =
      [i|2020-07-02 buy 100 AAPL 260.00
         2020-07-03 sell 100 AAPL 240.00 fees 0.20
         2020-07-04 buy 100 AAPL 260.00
        |]
    oo =
      [i|2020-07-02 00:00:00 buy 100 AAPL 260.0000 open
         2020-07-03 00:00:00 sell 100 AAPL 240.0000 fees 0.20 close loss 20.000000
         2020-07-04 00:00:00 buy 100 AAPL 260.0000 open washed 20.000000
        |]

(@-->) :: Text -> Text -> Assertion
x @--> y = do
  y' <- parseProcessPrint "" x
  trimLines y' @?= trimLines y
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
      case processJournal actions of
        Left err ->
          error $ "Error processing journal " ++ path ++ ": " ++ show err
        Right j -> pure $ printJournal j
