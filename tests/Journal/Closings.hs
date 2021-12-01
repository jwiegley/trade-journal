{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module Closings (testClosings) where

import Amount
import Control.Exception
import Control.Lens hiding (each)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Time
import Data.Typeable
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Closings
import Journal.Pipes ()
import Journal.Types
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Text.Show.Pretty hiding (Time)

{--------------------------------------------------------------------------}

data TestAction = TestAction
  { _action :: Annotated (Entry ()),
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

input :: State TestState a -> [Annotated (Entry ())]
input =
  map _action
    . filter _regular
    . _pending
    . flip execState (TestState [] 0)

trades :: State TestState a -> [Annotated (Entry ())]
trades =
  map (\x -> (x ^. action) & details <>~ (x ^. annotations))
    . _pending
    . flip execState (TestState [] 0)

instance (PrettyVal a, PrettyVal b) => PrettyVal (Map a b) where
  prettyVal m = Con "Map.fromList" (map prettyVal (M.assocs m))

data Positions = Positions
  { _positions :: IntMap (Position ()),
    _entries :: [Annotated (Entry ())]
  }

makeLenses ''Positions

getEntries :: MonadIO m => StateT Positions m b -> m [Annotated (Entry ())]
getEntries act = view entries <$> execStateT act (Positions mempty mempty)

instance PrettyVal () where
  prettyVal () = String "()"

checkJournal ::
  MonadIO m =>
  State TestState a ->
  StateT Positions m b ->
  StateT Positions m c ->
  m ()
checkJournal journal act actLeft = do
  let result = closings FIFO (input journal)
  entries' <- getEntries act
  entriesLeft' <- getEntries actLeft
  let expected = M.fromList [("FOO", entriesLeft')]
  result @?== (entries', expected)

{--------------------------------------------------------------------------}

bought :: MonadIO m => Annotated Lot -> StateT Positions m ()
bought b = entries <>= [Action . Buy <$> b]

sold :: MonadIO m => Annotated Lot -> StateT Positions m ()
sold s = entries <>= [Action . Sell <$> s]

open ::
  MonadIO m =>
  Int ->
  Disposition ->
  Annotated Lot ->
  StateT Positions m ()
open n disp b = do
  let pos = Position n (b ^. item) disp (b ^. item . price) ()
  positions . at n ?= pos
  entries <>= [Event (Open pos) <$ b]

close ::
  MonadIO m =>
  Int ->
  Annotated Lot ->
  Amount 6 ->
  StateT Positions m ()
close n b pl = do
  preuse (positions . ix n) >>= \case
    Nothing -> error $ "No open position " ++ show n
    Just pos -> do
      let pl' = case pos ^. posDisp of
            Long -> (b ^. item . price) - (pos ^. posBasis)
            Short -> (pos ^. posBasis) - (b ^. item . price)
      liftIO $ assertEqual' "closing gain/less" pl pl'
      entries <>= [Event (Close (Closing pos (b ^. item) ())) <$ b]
      let pos' = pos & posLot . amount -~ (b ^. item . amount)
          amt = pos' ^?! posLot . amount
      if amt < 0
        then error $ "Not enough shares in open position " ++ show n
        else
          if amt == 0
            then positions . at n .= Nothing
            else positions . at n ?= pos'

{--------------------------------------------------------------------------}

testClosings :: TestTree
testClosings =
  testGroup
    "closings"
    [ testProperty "buy-buy-buy" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                buy b
                buy b
                buy b
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
            )
            ( do
                open 1 Long b
                open 2 Long b
                open 3 Long b
            ),
      --
      testProperty "buy-buy-buy-sell-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                buy b
                buy b
                buy b
                sell b
                sell b
                sell b
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b
                close 1 b 0
                sold b
                close 2 b 0
                sold b
                close 3 b 0
            )
            (pure ()),
      --
      testProperty "buy-buy-buy-sell2" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            ( do
                buy b
                buy b
                buy b
                sell b2
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b2
                close 1 b 0
                close 2 b 0
            )
            ( do
                open 3 Long b
            ),
      --
      testProperty "buy-buy-buy-sell3" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b3 = b & item . amount *~ 3
          checkJournal
            ( do
                buy b
                buy b
                buy b
                sell b3
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b3
                close 1 b 0
                close 2 b 0
                close 3 b 0
            )
            (pure ()),
      --
      testProperty "buy-buy-buy-sell4" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b4 = b & item . amount *~ 4
          checkJournal
            ( do
                buy b
                buy b
                buy b
                sell b4
            )
            ( do
                bought b
                open 1 Long b
                bought b
                open 2 Long b
                bought b
                open 3 Long b
                sold b4
                close 1 b 0
                close 2 b 0
                close 3 b 0
                open 4 Short b
            )
            ( do
                open 4 Short b
            ),
      --
      testProperty "buy2-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            ( do
                buy b2
                sell b
            )
            ( do
                bought b2
                open 1 Long b2
                sold b
                close 1 b 0
            )
            ( do
                open 1 Long b
            ),
      --
      testProperty "buy2-sell-sell" $
        property $ do
          b <- forAll $ genAnnotated genLot
          let b2 = b & item . amount *~ 2
          checkJournal
            ( do
                buy b2
                sell b
                sell b
            )
            ( do
                bought b2
                open 1 Long b2
                sold b
                close 1 b 0
                sold b
                close 1 b 0
            )
            (pure ()),
      --
      testProperty "sell-sell-sell" $
        property $ do
          s <- forAll $ genAnnotated genLot
          checkJournal
            ( do
                sell s
                sell s
                sell s
            )
            ( do
                sold s
                open 1 Short s
                sold s
                open 2 Short s
                sold s
                open 3 Short s
            )
            ( do
                open 1 Short s
                open 2 Short s
                open 3 Short s
            )
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
