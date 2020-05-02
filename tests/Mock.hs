{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Mock where

import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Maybe (fromMaybe)
import           Data.Text
import           Data.Time
import           Data.Time.Format.ISO8601
import           ThinkOrSwim.API.TransactionHistory.GetTransactions (TransactionSubType(..))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Event

data Mock = Mock
    { _mockIdent      :: API.TransactionId
    , _mockTime       :: UTCTime
    , _mockKind       :: API.TransactionSubType
    , _mockCusip      :: Text
    , _mockSymbol     :: Text
    , _mockUnderlying :: Text
    , _mockQuantity   :: Amount 6
    , _mockCost       :: Amount 6
    , _mockFees       :: Amount 2
    }

instance Show Mock where
    show Mock {..} =
        show _mockQuantity
            ++ " @@ " ++ show (_mockCost / _mockQuantity)
            ++ " ## \"" ++ iso8601Show (utctDay _mockTime) ++ "\""

makeLenses ''Mock

newMock :: Mock
newMock = Mock
    { _mockIdent      = 0
    , _mockTime       = UTCTime (ModifiedJulianDay 0) 0
    , _mockKind       = BuyTrade
    , _mockCusip      = ""
    , _mockSymbol     = ""
    , _mockUnderlying = ""
    , _mockQuantity   = 0
    , _mockCost       = 0
    , _mockFees       = 0
    }

instance Transactional Mock where
    ident      = mockIdent
    time       = mockTime
    kind       = mockKind
    cusip      = mockCusip
    symbol     = mockSymbol
    underlying = mockUnderlying
    quantity   = mockQuantity
    cost       = mockCost
    fees       = mockFees

trade :: TransactionSubType -> Text -> Amount 6 -> Amount 6 -> Mock
trade k d q c = newMock
    & mockKind     .~ k
    & mockQuantity .~ q
    & mockCost     .~ c
    & mockTime     .~ UTCTime day 0
  where
    day = fromMaybe (error $ "Failed to parse: " ++ unpack d)
                    (iso8601ParseM (unpack d))

bto, stc :: Text -> Amount 6 -> Amount 6 -> Mock
bto = trade BuyTrade
stc = trade SellTrade

test_gainsKeeper :: IO ()
test_gainsKeeper = do
    let res = (`runState` []) $ do
            _ <- processTransaction (bto "2020-03-21" 10 20.00)
            processTransaction (stc "2020-03-22" 10 30.00)
    print res
    print $ res^?_1._head
