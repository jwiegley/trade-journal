{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans #-}

module TestAction where

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
import Data.Text (Text)
import Data.Time
import Data.Typeable
-- import Debug.Trace
import GHC.Generics hiding (to)
import Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Journal.Closings
import Journal.Pipes ()
import Journal.Types
import Test.HUnit.Lang (FailureReason (..))
import Test.Tasty.HUnit
import Text.Show.Pretty hiding (Time)

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

{--------------------------------------------------------------------------}

instance (PrettyVal a, PrettyVal b) => PrettyVal (Map a b) where
  prettyVal m = Con "Map.fromList" (map prettyVal (M.assocs m))

data Positions a = Positions
  { _positions :: IntMap (Position a),
    _entries :: [Annotated (Entry a)]
  }

newPositions :: Positions a
newPositions = Positions mempty mempty

makeLenses ''Positions

getEntries :: MonadIO m => StateT (Positions a) m r -> m [Annotated (Entry a)]
getEntries act = view entries <$> execStateT act newPositions

instance PrettyVal () where
  prettyVal () = String "()"

data TestExpr a
  = EBought (Annotated Lot)
  | ESold (Annotated Lot)
  | EOpen (Annotated (Position a))
  | EClose Int (Annotated Lot) (Amount 6) a
  deriving (Show, Eq, Generic, PrettyVal, Typeable)

makePrisms ''TestExpr

type TestDSL a = State [TestExpr a]

eval :: (Show a, MonadIO m) => TestExpr a -> StateT (Positions a) m ()
eval (EBought b) = entries <>= [Action . Buy <$> b]
eval (ESold s) = entries <>= [Action . Sell <$> s]
eval (EOpen p@(view item -> pos)) = do
  positions . at (pos ^. posIdent) ?= pos
  -- traceM $ "write " ++ show (pos ^. posIdent) ++ " => " ++ ppShow pos
  entries <>= [Event (Open pos) <$ p]
eval (EClose n s pl w) = do
  preuse (positions . ix n) >>= \case
    Nothing -> error $ "No open position " ++ show n
    Just pos -> do
      -- traceM $ "read " ++ show n ++ " => " ++ ppShow pos
      let pl' = case pos ^. posDisp of
            Long -> (s ^. item . price) - (pos ^. posBasis)
            Short -> (pos ^. posBasis) - (s ^. item . price)
      liftIO $ assertEqual' "closing gain/less" pl pl'
      entries <>= [Event (Close (Closing pos (s ^. item) w)) <$ s]
      entries' <- use entries
      -- traceM $ "entries' = " ++ ppShow entries'
      let pos' = pos & posLot . amount -~ (s ^. item . amount)
          amt = pos' ^?! posLot . amount
      if amt < 0
        then error $ "Not enough shares in open position " ++ show n
        else
          if amt == 0
            then positions . at n .= Nothing
            else positions . at n ?= pos'

evalDSL :: (Show a, MonadIO m) => TestDSL a () -> StateT (Positions a) m ()
evalDSL = mapM_ TestAction.eval . flip execState []

bought :: Annotated Lot -> TestDSL a ()
bought b = id <>= [EBought b]

sold :: Annotated Lot -> TestDSL a ()
sold s = id <>= [ESold s]

open ::
  Monoid a =>
  Int ->
  Disposition ->
  Annotated Lot ->
  TestDSL a ()
open n disp b =
  id
    <>= [ EOpen $
            Position
              { _posIdent = n,
                _posLot = b ^. item,
                _posDisp = disp,
                _posBasis = b ^. item . price,
                _posData = mempty
              }
              <$ b
        ]

close ::
  Monoid a =>
  Int ->
  Annotated Lot ->
  Amount 6 ->
  TestDSL a ()
close n b pl = id <>= [EClose n b pl mempty]

{--------------------------------------------------------------------------}

data TestAction a = TestAction
  { _action :: Annotated (Entry a),
    _annotations :: [Annotation],
    _regular :: Bool
  }

makeLenses ''TestAction

data TestState a = TestState
  { _pending :: [TestAction a],
    _balance :: Amount 2
  }

makeLenses ''TestState

buy :: Annotated Lot -> State (TestState a) ()
buy b =
  do
    -- bal <- use balance
    balance += c
    pending <>= [TestAction (Action . Buy <$> b) [] True]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced . to negate

sell :: Annotated Lot -> State (TestState a) ()
sell b =
  do
    -- bal <- use balance
    balance += c
    pending <>= [TestAction (Action . Sell <$> b) [] True]
  where
    c = ((b ^. item . amount) * (b ^. item . price)) ^. coerced

input :: State (TestState a) r -> [Annotated (Entry a)]
input =
  map _action
    . filter _regular
    . _pending
    . flip execState (TestState [] 0)

trades :: State (TestState a) r -> [Annotated (Entry a)]
trades =
  map (\x -> (x ^. action) & details <>~ (x ^. annotations))
    . _pending
    . flip execState (TestState [] 0)

checkJournal ::
  (Monoid a, Eq a, Show a, PrettyVal a, MonadIO m) =>
  ( ([Annotated (Entry a)], Map Text [Annotated (Entry a)]) ->
    ([Annotated (Entry a)], Map Text [Annotated (Entry a)])
  ) ->
  State (TestState a) r ->
  TestDSL a () ->
  TestDSL a () ->
  m ()
checkJournal f journal act actLeft = do
  entries' <- getEntries (evalDSL act)
  entriesLeft' <- getEntries (evalDSL actLeft)
  f (closings FIFO (input journal))
    @?== (entries', M.fromList [("FOO", entriesLeft')])
