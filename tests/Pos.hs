{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos where

import Control.Lens
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.State
import Data.Amount
import Data.Coerce
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Time
import Data.Time.Format.ISO8601
import Test.Tasty.HUnit as HUnit
import Text.Show.Pretty
import ThinkOrSwim.Transaction

data Equity = Equity
    { _qnt      :: Amount 4
    , _cst      :: Amount 4
    , _dy       :: Day
    , _lss      :: Amount 2
    , _xid      :: Int64
    , _deferred :: Maybe (Amount 2)
    , _eligible :: Bool
    }

instance Eq Equity where
    x == y = show (_qnt x) == show (_qnt y)
           && (show (_cst x) == show (_cst y) ||
              show (_cst x / abs (_qnt x)) ==
              show (_cst y / abs (_qnt y)))
           && show (_dy x)  == show (_dy y)
           && show (_lss x) == show (_lss y)

instance Show Equity where
    show Equity {..} =
        show _qnt ++ " @@ " ++ show (_cst / abs _qnt)
                  ++ " ## \"" ++ iso8601Show _dy ++ "\""
                  ++ " $$ " ++ show _lss

makeLenses ''Equity

newEquity :: Equity
newEquity = Equity
    { _qnt      = 0
    , _cst      = 0
    , _dy       = ModifiedJulianDay 0
    , _lss      = 0
    , _xid      = 0
    , _deferred = Nothing
    , _eligible = True
    }

(@@) :: Amount 4 -> Amount 4 -> Equity
q @@ c = newEquity & quantity .~ q & cost .~ abs q * c

(@@@) :: Amount 4 -> Amount 4 -> Equity
q @@@ c = newEquity & quantity .~ q & cost .~ c

(##) :: Equity -> Text -> Equity
p ## d = p & day
    .~ fromMaybe (error $ "Failed to parse: " ++ unpack d)
                 (iso8601ParseM (unpack d))

($$) :: Equity -> Amount 2 -> Equity
p $$ a = p & loss .~ a

instance Transactional Equity where
    symbol f p   = p <$ f "???"
    quantity     = qnt
    cost         = cst
    price f p    = p <$ f 0
    day          = dy
    loss         = lss
    washDeferred = deferred
    washEligible = eligible
    ident        = xid

    washLoss b x y | b || abs (x^.quantity) == abs (y^.quantity) =
        y & loss     .~ - (x^.loss)
          & cst      +~ coerce (x^.loss)
          & eligible .~ False
    washLoss _ _ y = y

    clearLoss = loss .~ 0

    isTransferIn _ = False

    arePaired x y = (x^.quantity < 0 && y^.quantity > 0)
                  || (x^.quantity > 0 && y^.quantity < 0)
    areEquivalent _ _ = True

    showPretty = show

assertEqual'
  :: (Eq a, Show a, HasCallStack)
  => String                      -- ^ The message prefix
  -> a                           -- ^ The expected value
  -> a                           -- ^ The actual value
  -> Assertion
assertEqual' preface expected actual =
    unless (actual == expected) $
        assertFailure (msg ppShow)
  where
    msg f = (if Prelude.null preface then "" else preface ++ "\n")
       ++ "expected:\n" ++ f expected
       ++ "\nbut got:\n" ++ f actual

(@?==) :: (Eq a, Show a) => StateT s IO a -> a -> StateT s IO ()
action @?== result = do
    res <- action
    lift $ assertEqual' "" result res

(@?=) :: (Eq a, Show a) => a -> a -> IO ()
action @?= result = do
    let res = action
    assertEqual' "" result res
