{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Model where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Amount
import Data.Int
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty
import ThinkOrSwim.Model

data MockFailure = MockFailure FailureReason
  deriving (Eq, Typeable)

instance Show MockFailure where
  show (MockFailure (Reason msg)) = msg
  show (MockFailure (ExpectedButGot preface expected actual)) = msg
    where
      msg =
        (case preface of Just str -> str ++ "\n"; Nothing -> "")
          ++ "expected:\n"
          ++ expected
          ++ "\n but got:\n"
          ++ actual

instance Exception MockFailure

assertEqual' ::
  (Eq a, Show a, HasCallStack) =>
  -- | The message prefix
  String ->
  -- | The expected value
  a ->
  -- | The actual value
  a ->
  Assertion
assertEqual' preface expected actual =
  unless (actual == expected) $ do
    throwIO
      ( MockFailure
          ( ExpectedButGot
              ( if Prelude.null preface
                  then Nothing
                  else Just preface
              )
              (ppShow expected)
              (ppShow actual)
          )
      )

infix 1 @?==

(@?==) :: (MonadIO m, Eq a, Show a, HasCallStack) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

testModel :: TestTree
testModel =
  testGroup
    "model"
    [ buy
    ]

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  pure $ UTCTime day (secondsToDiffTime secs)

genAmount :: MonadGen m => Range Int64 -> m (Amount n)
genAmount range = do
  d <- Gen.integral range
  n <- Gen.integral range
  pure $ Amount (d % n)

genLot :: MonadGen m => m Lot
genLot = do
  q <- genAmount (Range.linear 1 1000)
  t <- genUTCTime
  pure $ Lot q 1 t []

buy :: TestTree
buy =
  testGroup
    "buy"
    [ testProperty "simple-buy" $ property $ do
        b <- forAll genLot
        -- A simple buy
        lift $
          processLots [BuySell b]
            @?== ( [ SnocEvent (b & details <>~ [Position Open]),
                     Result (b & details <>~ [Position Open])
                   ],
                   [b & details <>~ [Position Open]]
                 ),
      testProperty "buy-sell-breakeven" $ property $ do
        b <- forAll genLot
        let s = b & amount %~ negate
        -- A buy and sell at break-even
        lift $
          processLots
            [ BuySell b,
              BuySell s
            ]
            @?== ( [ SnocEvent (b & details <>~ [Position Open]),
                     Result (b & details <>~ [Position Open]),
                     Result (s & details <>~ [GainLoss 0])
                   ],
                   [ b & details <>~ [Position Open],
                     s & details <>~ [GainLoss 0]
                   ]
                 ),
      testProperty "buy-sell-profit" $ property $ do
        b <- forAll genLot
        let s = b & amount %~ negate
            sp = s & price +~ 10
        -- A buy and sell at a profit
        lift $
          processLots
            [ BuySell b,
              BuySell sp
            ]
            @?== ( [ SnocEvent (b & details <>~ [Position Open]),
                     Result (b & details <>~ [Position Open]),
                     Result (sp & details <>~ [GainLoss 10])
                   ],
                   [ b & details <>~ [Position Open],
                     sp & details <>~ [GainLoss 10]
                   ]
                 ),
      testProperty "buy-sell-loss" $ property $ do
        b <- forAll genLot
        let s = b & amount %~ negate
            sl = s & price -~ 1
        -- A buy and sell at a loss
        lift $
          processLots
            [ BuySell b,
              BuySell sl
            ]
            @?== ( [ SnocEvent (b & details <>~ [Position Open]),
                     Result (b & details <>~ [Position Open]),
                     Result (sl & details <>~ [GainLoss (-1)]),
                     Submit (Wash (sl & details <>~ [GainLoss (-1)])),
                     SnocEvent (sl & details .~ [WashSaleAdjust (-1)]),
                     SubmitEnd
                   ],
                   [ b & details <>~ [Position Open],
                     sl & details <>~ [GainLoss (-1)]
                   ]
                 ),
      testProperty "simple-sell" $ property $ do
        b <- forAll genLot
        let s = b & amount %~ negate
        -- A simple sell
        lift $
          processLots [BuySell s]
            @?== ( [ SnocEvent (s & details <>~ [Position Open]),
                     Result (s & details <>~ [Position Open])
                   ],
                   [s & details <>~ [Position Open]]
                 ),
      testProperty "sell-buy-profit" $ property $ do
        b <- forAll genLot
        let s = b & amount %~ negate
        -- A sell and buy at a profit
        lift $
          processLots
            [ BuySell s,
              BuySell b
            ]
            @?== ( [ SnocEvent (s & details <>~ [Position Open]),
                     Result (s & details <>~ [Position Open]),
                     Result (b & details <>~ [GainLoss 0])
                   ],
                   [ s & details <>~ [Position Open],
                     b & details <>~ [GainLoss 0]
                   ]
                 )
    ]