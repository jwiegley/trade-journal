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
import Control.Arrow ((***))
import Control.Lens
import Control.Monad.State
import Data.Amount
import Data.Coerce
import Data.Foldable
import Data.Ledger as Ledger
import Data.Maybe (catMaybes, maybeToList)
import Data.Split
import Data.Text (Text)
import Data.Time
import Data.Tuple (swap)
import Prelude hiding (Float, Double, (<>))
import Text.PrettyPrint
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

import Debug.Trace (trace)

-- Given a history of closing and opening transactions (where the opening
-- transactions are identified as having a loss of 0.0), determine the washed
-- losses that should apply to either these are some pending set of opening
-- transactions in the GainsKeeperState, and the parts of those transactions
-- they apply to, possibly splitting up lots to record the tally. The state
-- and/or input value is then modified to reflect these adjustments.
--
-- Possible outcomes of calling this function:
--
-- 1. There are no wash sales, opening transactions are recorded to check for
--    future losses against them.
--
-- 2. Input contains opening transactions to which wash sales apply. Each is
--    split into N shares, and matched with shares of past applicable losses.
--    These past shares are removed from the history, and the cost basis
--    adjusted transactions plus any others are returned.
--
-- 3. Input contains closing transactions with losses, for which the wash sale
--    rule applies. These are added to the history of such losses and returned
--    in the output.
--
-- 4. A combination of 2 and 3, in which case 3 is added to the history, and
--    then the algorithm applies similarly to 2, except that the shares from 3
--    are also returned after having their losses adjusted.
--
-- Said another way, the inputs are as follows:
--
--   State: OPEN, RECENT  (open positions, and xacts within last 61 days)
--   Input: XACTS=CL+OP   (closing and opening xacts being processed)
--
-- - OPEN is ignored by this function.
-- - RECENT may be added to, changed or subtracted from. Transactions may
--   be split into multiple to account for transferred losses.
-- - CL may be split and/or changed to reduce losses.
-- - CL with losses (or their fractioned parts) may be added to RECENT.
-- - Transactions in OP may be split and/or changed to increase cost basis.
--
-- If called for a close that is not a loss, ignore.
--
-- If closing at a loss, and the corresponding open was <= 30 days ago, check
-- if a purchase of the same or equivalent security happened within the last
-- 30 days. If so, transfer the loss to that opening and record any remaining
-- loss; otherwise record the whole loss so it may be applied to future
-- openings within 30 days from the loss.
--
-- We're opening a transaction to which the wash sale rule may apply. Check
-- whether an applicable losing transaction was made within the last 30 days,
-- and if so, adjust the cost basis and remove the losing historical
-- transaction since wash sales are only applied once.
washSaleRule
    :: Text
    -> [(Bool, LotAndPL API.TransactionSubType API.Transaction)]
      -- ^ The boolean value is True if is an opening transaction.
    -> State (GainsKeeperState API.TransactionSubType API.Transaction)
            (Doc, [(Bool, LotAndPL API.TransactionSubType API.Transaction)])
washSaleRule underlying ls = zoom (positionEvents.at underlying.non []) $
    fmap ((vcat *** concat) . unzip) $ forM ls $ \(b, l) -> do
        events <- get

        let pr' = text "pl: " <> text (showLotAndPL l)
               $$ text "ev: " <> renderList (text . showLotAndPL) events

            rend d (n :: Int) r eb ea = d
               $$ text "e" <> text (show n) <> ": "
                      <> renderList (text . showLotAndPL) eb
               $$ text "E" <> text (show n) <> ": "
                      <> renderList (text . showLotAndPL) ea
               $$ text "-" <> text (show n) <> "> "
                      <> renderList (text . showLotAndPL) r

            c = wash False events l

            pr = pr'
                $$ text "c^.fromList    " <> renderList (text . showLotAndPL) (c^.fromList)
                $$ text "c^.newList     " <> renderList (text . showLotAndPL) (c^.newList)
                $$ text "c^.fromElement " <> renderList (text . showLotAndPL) (c^.fromElement)
                $$ text "c^.newElement  " <> text (show (fmap showLotAndPL (c^.newElement)))

            -- If the result of calling 'wash' is a series of washed losses,
            -- check if there are other openings they could be applied to.
            (doc, _, fr, fhs) =
                (\f -> foldl' f (pr, 1, [], c^.newList)
                               (c^.fromElement)) $ \(d, n, r, hs) e ->
                    let i  = wash True hs e
                        r' = r -- ++ i^.fromElement
                               ++ i^.fromList
                               ++ maybeToList (i^.newElement)
                        h' = i^.newList
                    in ( rend d n r' hs h'
                       , succ n
                       , r'
                       , h' )

            res = fr ++ maybeToList (c^.newElement)
            evs = fhs ++ fr ++ case c^.newElement of
                Just e | e^.plLoss == 0 -> [e]
                _ -> []

        put evs

        pure (rend doc 0 res events evs, (b,) <$> res)

transferLoss
    :: LotAndPL TransactionSubType API.Transaction
    -> LotAndPL TransactionSubType API.Transaction
    -> ( LotAndPL TransactionSubType API.Transaction
      , LotAndPL TransactionSubType API.Transaction )
transferLoss x y
    | abs (x^.plLot.quantity) == abs (y^.plLot.quantity) =
    ( x & plLoss .~ 0
    , y & plKind .~ WashLoss
        & plLoss .~ - (x^.plLoss)
        & plLot.Ledger.cost._Just +~ coerce (x^.plLoss)
        & plLot.refs <>~ [ Ref (WashSaleRule (coerce (x^.plLoss)))
                               (x^?!plLot.refs._head.refId)
                               (x^?!plLot.refs._head.refOrig) ]
    )
transferLoss x y = (x, y)

-- Assumes 'pl' is received in temporal order.
wash :: Bool
     -> [LotAndPL TransactionSubType API.Transaction]
     -> LotAndPL TransactionSubType API.Transaction
     -> Considered (LotAndPL TransactionSubType API.Transaction)
                  (LotAndPL TransactionSubType API.Transaction)
                  (LotAndPL TransactionSubType API.Transaction)
wash inverted hs pl =
    consider f mk (Just <$> filter (within 30 pl) hs) pl
        & fromList %~ catMaybes
        & newList  %~ catMaybes
  where
    aligned   h x = uncurry splits (alignPL h x) & src._Splits %~ Just
    unaligned h x = nothingApplied @() (Just h) x

    f Nothing _ = error "Unexpected"
    f (Just h) x
        | not (h^.plLot.Ledger.instrument == Ledger.Equity &&
             x^.plLot.Ledger.instrument == Ledger.Equity &&
             (h^.plLot) `pairedCommodityLots` (x^.plLot)) =
              trace ("h.1 = " ++ showLotAndPL h) $
              trace ("x.1 = " ++ showLotAndPL x) $
              unaligned h x

        | if inverted then opening else closing =
              trace ("h.2 = " ++ showLotAndPL h) $
              trace ("x.2 = " ++ showLotAndPL x) $
              aligned h x

        | if inverted then closing else opening =
              trace ("h.3 = " ++ showLotAndPL h) $
              trace ("x.3 = " ++ showLotAndPL x) $
              let res = aligned h x
                  tr  = liftA2 transferLoss in
              case (if inverted then flip else id)
                       tr (res^?src._SplitUsed._Just)
                          (res^?dest._SplitUsed) of
                  Just (if inverted then swap else id -> (ud, us)) ->
                      res & src._SplitUsed._Just .~ ud
                          & dest._SplitUsed        .~ us
                  Nothing -> res

        | otherwise =
              trace ("h.4 = " ++ showLotAndPL h) $
              trace ("x.4 = " ++ showLotAndPL x) $
              unaligned h x
      where
        closing = h^.plLoss == 0 && x^.plLoss >  0
        opening = h^.plLoss >  0 && x^.plLoss == 0

    mk _ _ = id

    within n (view plDay -> Just x) (view plDay -> Just y) =
        abs (x `diffDays` y) <= n
    within _ _ _ = False
