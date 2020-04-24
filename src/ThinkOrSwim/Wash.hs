{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- The "Wash Sale Rule"

Summary:

    A. Buy N shares of STCK
           |
           |  up to 30 days
           v
    B. Sell <=A shares of STCK at a loss
           |
           |  up to 30 days, at anytime within these 61 days
           v
    C. Buy <=B shares of "STCK equivalent"

    Example 1: Buy 100 shares, sell 50, buy 100: first 50 shares of those 100
    repurchased have their cost basis adjusted to include the loss from the
    sale of 50.

    Example 2: Buy 100 shares, buy 50, sell 100: the 50 shares in the second
    purchase have their cost basis adjusted to include the loss from the sale
    of the original 100.

    If this situation holds, the loss incurred by X'' shares from B is added
    to the cost basis of the X'' shares of "STCK equivalent" repurchased. This
    only happens once.

    Note: this includes the day of sale, therefore the duration C - A may be
    up 61 days in length.

26 U.S. Code § 1091. Loss from wash sales of stock or securities

(a) Disallowance of loss deduction

    In the case of any loss claimed to have been sustained from any sale or
    other disposition of shares of stock or securities where it appears that,
    within a period beginning 30 days before the date of such sale or
    disposition and ending 30 days after such date, the taxpayer has acquired
    (by purchase or by an exchange on which the entire amount of gain or loss
    was recognized by law), or has entered into a contract or option so to
    acquire, substantially identical stock or securities, then no deduction
    shall be allowed under section 165 unless the taxpayer is a dealer in
    stock or securities and the loss is sustained in a transaction made in the
    ordinary course of such business. For purposes of this section, the term
    “stock or securities” shall, except as provided in regulations, include
    contracts or options to acquire or sell stock or securities.

(b) Stock acquired less than stock sold

    If the amount of stock or securities acquired (or covered by the contract
    or option to acquire) is less than the amount of stock or securities sold
    or otherwise disposed of, then the particular shares of stock or
    securities the loss from the sale or other disposition of which is not
    deductible shall be determined under regulations prescribed by the
    Secretary.

(c) Stock acquired not less than stock sold

    If the amount of stock or securities acquired (or covered by the contract
    or option to acquire) is not less than the amount of stock or securities
    sold or otherwise disposed of, then the particular shares of stock or
    securities the acquisition of which (or the contract or option to acquire
    which) resulted in the nondeductibility of the loss shall be determined
    under regulations prescribed by the Secretary.

(d) Unadjusted basis in case of wash sale of stock

    If the property consists of stock or securities the acquisition of which
    (or the contract or option to acquire which) resulted in the
    nondeductibility (under this section or corresponding provisions of prior
    internal revenue laws) of the loss from the sale or other disposition of
    substantially identical stock or securities, then the basis shall be the
    basis of the stock or securities so sold or disposed of, increased or
    decreased, as the case may be, by the difference, if any, between the
    price at which the property was acquired and the price at which such
    substantially identical stock or securities were sold or otherwise
    disposed of.

(e) Certain short sales of stock or securities and securities futures
    contracts to sell

    Rules similar to the rules of subsection (a) shall apply to any loss
    realized on the closing of a short sale of (or the sale, exchange, or
    termination of a securities futures contract to sell) stock or securities
    if, within a period beginning 30 days before the date of such closing and
    ending 30 days after such date --

      (1) substantially identical stock or securities were sold, or

      (2) another short sale of (or securities futures contracts to sell)
          substantially identical stock or securities was entered into.

    For purposes of this subsection, the term “securities futures contract”
    has the meaning provided by section 1234B(c).

(f) Cash settlement

    This section shall not fail to apply to a contract or option to acquire or
    sell stock or securities solely by reason of the fact that the contract or
    option settles in (or could be settled in) cash or property other than
    such stock or securities.


IRS Publication 550, page 59 states:

You cannot deduct losses from sales or trades of stock or securities in a wash
sale unless the loss was incurred in the ordinary course of your business as a
dealer in stock or securities. A wash sale occurs when you sell or trade stock
or securities at a loss and within 30 days before or after the sale you:

  - Buy substantially identical stock or securities,
  - Acquire substantially identical stock or securities in a fully taxable
    trade,
  - Acquire a contract or option to buy substantially identical stock or
    securities, or
  - Acquire substantially identical stock for your individual retirement
    account (IRA) or Roth IRA.

If you sell stock and your spouse or a corporation you control buys
substantially identical stock, you also have a wash sale.

If your loss was disallowed because of the wash sale rules, add the disallowed
loss to the cost of the new stock or securities (except in (4) above). The
result is your basis in the new stock or securities. This adjustment postpones
the loss deduction until the disposition of the new stock or securities. Your
holding period for the new stock or securities includes the holding period of
the stock or securities sold.


https://www.tradelogsoftware.com/resources/wash-sales/#wash-sale-short-sales

Which Trades Can Trigger a Wash Sale?

The following tables show the many possible trade combinations that can
trigger a wash sale and that are fully supported by the code below.

Buy Stock then Sell at a Loss
  - Buy Stock on Same Ticker
  - Buy Call for Same Ticker

Buy Call Option, Sell or Close at a Loss
  - Buy Stock on Same Ticker
  - Buy Call for Same Ticker

Buy Put Option, Sell or Close at a Loss
  - Buy Stock on Same Ticker
  - Buy Call for Same Ticker
  - Buy Put for Same Ticker

Sell Stock Short then Buy to Cover at a Loss
  - Buy Stock on Same Ticker
  - Sell Stock on Same Ticker
  - Buy Call for Same Ticker
  - Sell Call for Same Ticker
  - Buy Put for Same Ticker
  - Sell Put for Same Ticker

Sell Call (Writer) then Close at a Loss
  - Buy Stock on Same Ticker
  - Sell Stock on Same Ticker
  - Buy Call for Same Ticker
  - Sell Call for Same Ticker
  - Buy Put for Same Ticker
  - Sell Put for Same Ticker

Sell Put (Writer) then Close at a Loss
  - Buy Stock on Same Ticker
  - Sell Stock on Same Ticker
  - Buy Call for Same Ticker
  - Sell Call for Same Ticker
  - Buy Put for Same Ticker
  - Sell Put for Same Ticker

-}

module ThinkOrSwim.Wash (washSaleRule) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Coerce
import Data.Foldable
import Data.Maybe (maybeToList)
import Data.Split
import Data.Time
import Data.Utils
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint as P
import ThinkOrSwim.Transaction

-- import Debug.Trace

-- This function assumes 'l' is received in temporal order. For this reason,
-- we remove all entries older than 30 days from the list before beginning.
--
-- If 'l' is an opening transaction:
--
--   a. No past losses apply, add it to the history, return it.
--   b. A past loss (1) applies, reprice the open, remove (1), add the
--      repriced transaction to the history and return it.
--
-- If 'l' is a closing transaction:
--
--   a. No past openings apply, just return it.
--   b. A past opening (1) applies, add it to the history, remove (1), and
--      return the transaction.
--   c. A past opening (1) applies and there is another past opening (2) to
--      which the loss applies. Remove (1) and (2), generate a sale/repurchase
--      of (2), remove (1). Add the repriced (2) back to the history and
--      return the input transaction with its loss removed along with (2).

washSaleRule :: Transactional a => a -> State [a] [a]
washSaleRule l
    | l^.loss == 0 = do

      events <- use id
      let c   = matchEvents events l wash
          res = c^.fromElement ++ maybeToList (c^.newElement)
      id .= c^.newList ++ map clearLoss res
      pure res

    | l^.loss > 0 = do

      events <- use id
      let c = matchEvents events l id
      case c^.fromElement of
          [] -> do
              id .= c^.newList
              pure [l]

          xs -> do
              (y, ys, zs, nl) <- (\f -> foldlM f (l, [], [], c^.newList) xs) $
                  \(y, ys, zs, nl) x -> do
                      let d = matchEvents nl x id
                          (w, reverse -> fl) = foldl' transferLoss (y, [])
                              (spreadAmounts (^.quantity)
                                 (sum (d^..fromElement.traverse.loss))
                                 (d^.fromList))
                      pure ( w
                           , ys ++ (d^.fromList & each.quantity %~ negate)
                           , zs ++ fl
                           , d^.newList ++ maybeToList (d^.newElement)
                           )
              id .= map clearLoss zs ++ nl
              pure (y:intermix ys zs)

    | otherwise = pure [l]
  where
    transferLoss (z, ys) (n, y) =
        ( z & loss +~ - n
        , (y & cost +~ coerce n
             & loss +~ - n) : ys
        )

    wash = zipped (src._SplitUsed) (dest._SplitUsed)
        %~ \(x, y) -> (x, washLoss x y)

    showPrettyList = show . renderList (text . showPretty)

    intermix [] [] = []
    intermix (x:xs) (y:ys) = x:y:intermix xs ys
    intermix xs ys = error $ "intermix called with uneven lengths: "
        ++ showPrettyList xs ++ " and " ++ showPrettyList ys

-- Given a list of transactional elements, and some candidate, find all parts
-- of elements in the first list that "match up" with the candidate. The
-- function 'k' offers an opportunity to modify the parts used and not used in
-- the source and destination at each step.
--
-- Example: [10, 20, 30, 40], 50
-- Result: Considered
--     _fromList    = [10, 20, 20]
--     _newList     =         [10, 40]
--     _fromElement = [50]
--     _newElement  = Nothing
--
-- Example: [10, 20, 30, 40], 200
-- Result: Considered
--     _fromList    = [10, 20, 30, 40]
--     _newList     = []
--     _fromElement =     [100]
--     _newElement  = Just 100

matchEvents :: Transactional a
            => [a] -> a -> (Applied () a a -> Applied () a a)
            -> Considered a a a
matchEvents hs pl k =
    consider f (\_ () -> id) (filter (within 30 pl) hs) pl
  where
    within n x y = abs ((x^.day) `diffDays` (y^.day)) <= n

    f h x | areEquivalent h x && (opening || closing) =
                k (uncurry splits (alignLots h x))
          | otherwise = nothingApplied @() h x
      where
        closing = h^.loss == 0 && x^.loss >  0
        opening = h^.loss >  0 && x^.loss == 0


