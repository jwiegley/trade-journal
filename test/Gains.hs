{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Gains (testGains) where

import Amount
import Control.Exception
import Control.Lens hiding (each)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Gains
import Journal.Pipes ()
import Journal.Types
import Pipes
import qualified Pipes.Prelude as P
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty hiding (Time)

{--------------------------------------------------------------------------}

data TestAction = TestAction
  { _action :: Annotated Entry,
    _annotations :: [Annotation],
    _regular :: Bool
  }

makeLenses ''TestAction

data TestState = TestState
  { _pending :: [TestAction],
    _balance :: Amount 2
  }

makeLenses ''TestState

buy :: Annotated Lot -> State TestState ()
buy b =
  do
    -- bal <- use balance
    balance += c
    pending <>= [TestAction (Action . Buy <$> b) [] True]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced . to negate

sell :: Annotated Lot -> State TestState ()
sell b =
  do
    -- bal <- use balance
    balance += c
    pending <>= [TestAction (Action . Sell <$> b) [] True]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced

input :: State TestState a -> [Annotated Entry]
input =
  map _action
    . filter _regular
    . _pending
    . flip execState (TestState [] 0)

trades :: State TestState a -> [Annotated Entry]
trades =
  map (\x -> (x ^. action) & details <>~ (x ^. annotations))
    . _pending
    . flip execState (TestState [] 0)

checkJournal ::
  MonadIO m =>
  State TestState a ->
  [Annotated Entry] ->
  m ()
checkJournal journal expected = do
  xs <- P.toListM (each (input journal) >-> gains FIFO)
  xs @?== expected

{--------------------------------------------------------------------------}

bought :: Functor f => f Lot -> f Entry
bought b = Action . Buy <$> b

sold :: Functor f => f Lot -> f Entry
sold s = Action . Sell <$> s

open ::
  Disposition ->
  Annotated Lot ->
  Int ->
  Annotated Entry
open disp b n = Event . Open disp <$> b & details <>~ [Idents [n]]

close ::
  Disposition ->
  Annotated Lot ->
  Int ->
  Amount 6 ->
  Annotated Entry
close disp b n pl =
  Event . flip (Close disp) pl <$> b & details <>~ [Idents [n]]

{--------------------------------------------------------------------------}

testGains :: TestTree
testGains =
  testGroup
    "gains"
    [ testProperty "buy" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                buy b
            )
            [ bought b,
              open Long b 1
            ],
      --
      testProperty "buy-buy" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                buy b
                buy b
            )
            [ bought b,
              open Long b 1,
              --
              bought b,
              open Long b 2
            ],
      --
      testProperty
        "sell"
        $ property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                sell s
            )
            [ sold s,
              open Short s 1
            ],
      --
      testProperty
        "sell-sell"
        $ property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                sell s
                sell s
            )
            [ sold s,
              open Short s 1,
              --
              sold s,
              open Short s 2
            ],
      --
      testProperty
        "buy-sell-breakeven"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                buy b
                sell b
            )
            [ bought b,
              open Long b 1,
              --
              sold b,
              close Long b 1 0
            ],
      --
      testProperty
        "buy-sell-profit"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let sp = b & item . price +~ 10
          checkJournal
            ( do
                buy b
                sell sp
            )
            [ bought b,
              open Long b 1,
              --
              sold sp,
              close Long b 1 10
            ],
      --
      testProperty
        "buy-sell-part-profit"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
              sp = b & item . price +~ 10
          checkJournal
            ( do
                buy b2
                sell sp
            )
            [ bought b2,
              open Long b2 1,
              --
              sold sp,
              close Long b 1 10
              --
            ],
      --
      testProperty
        "buy-sell-some"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            ( do
                buy b2
                sell b
            )
            [ bought b2,
              open Long b2 1,
              --
              sold b,
              close Long b 1 0
            ],
      --
      testProperty
        "buy-sell-more"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            ( do
                buy b
                sell b2
            )
            [ bought b,
              open Long b 1,
              --
              sold b,
              close Long b 1 0,
              sold b,
              open Short b 2
            ],
      --
      testProperty
        "buy-sell-loss"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let sl = b & item . price -~ 1
          checkJournal
            ( do
                buy b
                buy b
                sell sl
            )
            [ bought b,
              open Long b 1,
              --
              bought b,
              open Long b 2,
              --
              sold sl,
              close Long b 1 (-1)
            ],
      --
      testProperty
        "sell-buy-profit"
        $ property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                sell s
                buy s
            )
            [ sold s,
              open Short s 1,
              --
              bought s,
              close Short s 1 0
            ],
      --
      testProperty
        "buy-buy-sell-loss"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let sl = b & item . price -~ 1
          checkJournal
            ( do
                buy b
                sell sl
            )
            [ bought b,
              open Long b 1,
              --
              sold sl,
              close Long b 1 (-1)
            ],
      --
      testProperty
        "buy-sell-loss-buy"
        $ property $ do
          b <- forAll $ genAnnotated genLot
          let sl = b & item . price -~ 1
          checkJournal
            ( do
                buy b
                sell sl
                buy b
            )
            [ bought b,
              open Long b 1,
              --
              sold sl,
              close Long b 1 (-1),
              --
              bought b,
              open Long b 2
            ]
    ]

{--------------------------------------------------------------------------}

newtype MockFailure = MockFailure FailureReason
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
  (Eq a, PrettyVal a, HasCallStack) =>
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
              (dumpStr expected)
              (dumpStr actual)
          )
      )

infix 1 @?==

(@?==) :: (MonadIO m, Eq a, PrettyVal a, HasCallStack) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual

{--------------------------------------------------------------------------}

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  pure $ UTCTime day (secondsToDiffTime secs)

genAmount :: MonadGen m => Range Integer -> m (Amount n)
genAmount range = do
  d <- Gen.integral range
  n <- Gen.integral range
  pure $ Amount (d % n)

genAnnotated :: MonadGen m => m a -> m (Annotated a)
genAnnotated gen = do
  _time <- genUTCTime
  _item <- gen
  let _details = []
  pure Annotated {..}

genLot :: MonadGen m => m Lot
genLot = do
  q <- genAmount (Range.linear 1 1000)
  p <- genAmount (Range.linear 1 2000)
  pure $ Lot q "FOO" p
