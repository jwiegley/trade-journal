{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mock where

import           Control.Arrow
import           Control.Lens hiding (assign)
import           Control.Monad.Morph
import           Control.Monad.State
import           Data.Amount
import           Data.Maybe (fromMaybe)
import           Data.Text as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           Data.Utils
import           Prelude hiding (Double, Float, (<>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.PrettyPrint
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
    rendered Mock {..} =
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

trade :: TransactionSubType -> Instruction -> Text -> Amount 6 -> Amount 6 -> Mock
trade k i d q c = newMock
    & mockKind        .~ k
    & mockInstruction ?~ i
    & mockQuantity    .~ q
    & mockCost        .~ c
    & mockTime        .~ UTCTime day 0
  where
    day = fromMaybe (error $ "Failed to parse: " ++ unpack d)
                    (iso8601ParseM (unpack d))

bto, stc :: Text -> Amount 6 -> Amount 6 -> Mock
bto = trade BuyTrade Buy
stc = trade SellTrade Sell

test_gainsKeeper :: IO ()
test_gainsKeeper = do
    let res = (`runState` newGainsKeeperState) $ do
            _ <- gainsKeeper newOptions (bto "2020-03-21" 10 20.00)
            gainsKeeper newOptions (stc "2020-03-22" 10 30.00)
    print res
    print $ res^?_1._head

data MockState = MockState
    { _mockState  :: GainsKeeperState Mock
    , _mockSpace  :: Text
    , _mockNextId :: API.TransactionId
    }
    deriving (Eq, Show)

makeLenses ''MockState

newMockState :: Text -> MockState
newMockState sym = MockState
    { _mockState  = newGainsKeeperState
    , _mockSpace  = sym
    , _mockNextId = 1
    }

withMockState :: Text -> StateT MockState IO a -> IO (a, GainsKeeperState Mock)
withMockState sym =
    fmap (second _mockState) . flip runStateT (newMockState sym)

withMockState_ :: Text -> StateT MockState IO a -> Assertion
withMockState_ sym action = () <$ withMockState sym action

submit :: Mock -> StateT MockState IO [Event (Lot Mock)]
submit m = do
    nextId <- use mockNextId
    mockNextId += 1
    sym <- use mockSpace
    let mock = m & mockSymbol     %~ (\x -> if T.null x then sym else x)
                 & mockUnderlying .~ sym
                 & mockIdent      .~ nextId
    zoom mockState $
        hoist (pure . runIdentity) $
            gainsKeeper newOptions mock

buy :: Amount 6 -> Amount 6 -> StateT MockState IO [Event (Lot Mock)]
buy = (submit .) . trade BuyTrade Buy "2020-03-01"

sell :: Amount 6 -> Amount 6 -> StateT MockState IO [Event (Lot Mock)]
sell = (submit .) . trade SellTrade Sell "2020-03-01"

assign :: Amount 6 -> Amount 6 -> StateT MockState IO [Event (Lot Mock)]
assign = (submit .) . trade OptionAssignment Buy "2020-03-01"
-- ^ jww (2020-05-04): Should be Buy if a Call, Sell if a Put, and it must
--   generate two OptionAssignment transactions, one for the option and one
--   for the equity. Lastly, costs for the option are * multiplier.

expire :: Amount 6 -> Amount 6 -> StateT MockState IO [Event (Lot Mock)]
expire = (submit .) . trade OptionAssignment Buy "2020-03-01"

option :: Text -> StateT MockState IO a -> StateT MockState IO a
option sym action = do
    prev <- use mockSpace
    mockSpace .= sym
    res <- action
    mockSpace .= prev
    pure res

gainsTest :: String -> StateT MockState IO a -> TestTree
gainsTest label = testCase label . withMockState_ "ZM"

testMock :: TestTree
testMock = gainsTest "mock-basic" $ do
    [b] <- buy 100 45.00
    [s] <- sell 100 50.00
    lift $ s^.gain @?= (s^.costs + b^.costs)^.coerced

tester :: Assertion
tester = withMockState_ "ZM" $ do
    [b1] <- buy 100 85.80
    buy 100 90.00
    [s1] <- sell 100 92.00
    lift $ s1^.gain @?= (b1^.costs - s1^.costs)^.coerced
    option "ZM_032020C95" $ do
        y <- buy 1 40.00
        assign 1 1.36
        expire 1 1.80
        x <- buy 2 50.00
        lift $ x @?= y
