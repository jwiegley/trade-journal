{-# LANGUAGE TemplateHaskell #-}

-- TODO:
--
-- - represent the greeks
-- - define composite strategies (condors, spreads)
-- - equivalent positions (S = C - P)
--   https://www.investopedia.com/articles/optioninvestor/09/equivalent-positions.asp
-- - pricing models (Black-Scholes, binomial)

module ThinkOrSwim.Options where

import Control.Lens
import Data.Fixed
import Data.Text
import Data.Time

data Side = Put | Call
    deriving (Eq, Ord, Show, Enum)

makePrisms ''Side

type Symbol = Text
type Amount = Fixed E2

data Contract = Contract
    { _side       :: Side
    , _strike     :: Amount
    , _expiration :: UTCTime
    , _underlying :: Symbol
    , _premium    :: Amount
    , _quantity   :: Int         -- long is > 0, short is < 0
    }
    deriving (Eq, Ord, Show)

makeClassy ''Contract

isLong :: Contract -> Bool
isLong o = o^.quantity > 0

breakEven :: Contract -> Amount
breakEven o = case o^.side of
    Call -> o^.strike + o^.premium
    Put  -> o^.strike - o^.premium

profit :: Amount -> Contract -> Amount
profit p o = case o^.side of
    Call -> p - breakEven o
    Put  -> breakEven o - p
