{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mock where

import           Control.Arrow
import           Control.Exception
import           Control.Lens hiding (assign)
import           Control.Monad.Morph
import           Control.Monad.State
import           Data.Amount
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Ratio
import           Data.Text as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           Data.Typeable
import           Data.Utils
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Prelude hiding (Double, Float, (<>))
import           Test.HUnit.Lang (FailureReason(..))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           Text.PrettyPrint as P
import           ThinkOrSwim.API.TransactionHistory.GetTransactions
                     (TransactionSubType(..), AssetType(..), Instruction(..))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Event
import           ThinkOrSwim.Options

data Mock = Mock
    { _mockIdent       :: API.TransactionId
    , _mockTime        :: UTCTime
    , _mockKind        :: API.TransactionSubType
    , _mockInstruction :: Maybe API.Instruction
    , _mockCusip       :: Text
    , _mockSymbol      :: Text
    , _mockUnderlying  :: Text
    , _mockAsset       :: AssetType
    , _mockQuantity    :: Amount 6
    , _mockCost        :: Amount 6
    , _mockFees        :: Amount 2
    }
    deriving (Eq, Ord, Show)

instance Render Mock where
    rendered Mock {..} = parens $
        tshow _mockQuantity
            <> " @@ " <> tshow (_mockCost / _mockQuantity)
            <> " ## " <> doubleQuotes (rendered _mockTime)

makeLenses ''Mock

newMock :: Mock
newMock = Mock
    { _mockIdent       = 0
    , _mockTime        = UTCTime (ModifiedJulianDay 0) 0
    , _mockKind        = BuyTrade
    , _mockInstruction = Nothing
    , _mockCusip       = ""
    , _mockSymbol      = ""
    , _mockUnderlying  = ""
    , _mockAsset       = API.Equity
    , _mockQuantity    = 0
    , _mockCost        = 0
    , _mockFees        = 0
    }

instance Transactional Mock where
    ident       = mockIdent
    time        = mockTime
    kind        = mockKind
    instruction = mockInstruction
    cusip       = mockCusip
    symbol      = mockSymbol
    underlying  = mockUnderlying
    asset       = mockAsset
    quantity    = mockQuantity
    cost        = mockCost
    fees        = mockFees
    distance t f s =
        f (abs ((t^.mockTime.to utctDay) `diffDays` (s^.mockTime.to utctDay)))
            <&> \x -> s & time %~
                     \(UTCTime _dy tm) ->
                         UTCTime (addDays (- x) (t^.time.to utctDay)) tm

data MockState = MockState
    { _mockState   :: GainsKeeperState Mock
    , _mockSpace   :: Text
    , _mockNextId  :: API.TransactionId
    , _mockOptions :: Options
    }
    deriving (Eq, Show)

makeLenses ''MockState

newMockState :: Text -> MockState
newMockState sym = MockState
    { _mockState   = newGainsKeeperState
    , _mockSpace   = sym
    , _mockNextId  = 1
    , _mockOptions = newOptions
    }

withMockState :: Monad m
              => Text -> StateT MockState m a -> m (a, GainsKeeperState Mock)
withMockState sym =
    fmap (second _mockState) . flip runStateT (newMockState sym)

withMockState_ :: Monad m => Text -> StateT MockState m a -> m ()
withMockState_ sym action = () <$ withMockState sym action

submit :: Monad m => Mock -> StateT MockState m [Event (Lot Mock)]
submit m = do
    nextId <- use mockNextId
    mockNextId += 1
    sym <- use mockSpace
    let mock = m & mockSymbol     %~ (\x -> if T.null x then sym else x)
                 & mockUnderlying .~ sym
                 & mockIdent      .~ nextId
    opts <- use mockOptions
    zoom mockState $
        hoist (pure . runIdentity) $
            gainsKeeper opts mock

trade :: TransactionSubType -> Instruction -> Text -> Amount 6 -> Amount 6 -> Mock
trade k i d q c = newMock
    & mockKind        .~ k
    & mockInstruction ?~ i
    & mockQuantity    .~ q
    & mockCost        .~ c * q
    & mockTime        .~ UTCTime day 0
  where
    day = fromMaybe (error $ "Failed to parse: " ++ unpack d)
                    (iso8601ParseM (unpack d))

buy :: Amount 6 -> Amount 6 -> Mock
buy = trade BuyTrade Buy "2020-03-01"

sell :: Amount 6 -> Amount 6 -> Mock
sell = trade SellTrade Sell "2020-03-01"

assign :: Amount 6 -> Amount 6 -> Mock
assign = trade OptionAssignment Buy "2020-03-01"
-- ^ jww (2020-05-04): Should be Buy if a Call, Sell if a Put, and it must
--   generate two OptionAssignment transactions, one for the option and one
--   for the equity. Lastly, costs for the option are * multiplier.

expire :: Amount 6 -> Amount 6 -> Mock
expire = trade OptionAssignment Buy "2020-03-01"

option :: Monad m => Text -> StateT MockState m a -> StateT MockState m a
option sym action = do
    prev <- use mockSpace
    mockSpace .= sym
    res <- action
    mockSpace .= prev
    pure res

gainsTest :: String -> StateT MockState IO a -> TestTree
gainsTest str = testCase str . withMockState_ "ZM"

gainsProperty :: String -> StateT MockState (PropertyT IO) a -> TestTree
gainsProperty str = testProperty str . property . withMockState_ "ZM"

(^+) :: Mock -> Mock -> Amount n
x ^+ y = (x^.cost + y^.cost)^.coerced

(^-) :: Mock -> Mock -> Amount n
x ^- y = (x^.cost - y^.cost)^.coerced

testMock :: TestTree
testMock = gainsTest "mock-basic" $ do
    let b = buy 100 (-45.00)
    submit b
    let s = sell 100 50.00
    [e] <- submit s
    e^.gain @?== b ^+ s

amount :: MonadGen m => Range Int64 -> m (Amount n)
amount range = do
    d <- Gen.integral range
    n <- Gen.integral range
    pure $ Amount (d % n)

wide :: MonadGen m => m (Amount n)
wide = amount (Range.linear 1 1000)

big :: MonadGen m => m (Amount n)
big  = amount (Range.linear 100 1000)

little :: MonadGen m => m (Amount n)
little = amount (Range.linear 1 50)

data MockFailure = MockFailure FailureReason
    deriving (Eq, Typeable)

instance Show MockFailure where
    show (MockFailure (Reason msg)) = msg
    show (MockFailure (ExpectedButGot preface expected actual)) = msg
      where
        msg = (case preface of Just str -> str ++ "\n"; Nothing -> "")
            ++ "expected:\n" ++ expected
            ++ "\n but got:\n" ++ actual

instance Exception MockFailure

assertEqual'
  :: (Eq a, Render a, HasCallStack)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqual' preface expected actual =
    unless (actual == expected) $ do
        throwIO (MockFailure
                 (ExpectedButGot
                    (if Prelude.null preface
                     then Nothing
                     else Just preface)
                    (render ("    " <> rendered expected))
                    (render ("    " <> rendered actual))))

infix 1 @?==
(@?==) :: (MonadIO m, Eq a, Render a, HasCallStack) => a -> a -> m ()
actual @?== expected = liftIO $ assertEqual' "" expected actual
