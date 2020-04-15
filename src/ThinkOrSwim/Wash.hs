{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
    of 50 out of the original 100.

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
trigger a wash sale and that are fully supported by the code below:

Buy Stock then Sell at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker
  Buy Put for Same Ticker
  Sell Put for Same Ticker

Buy Call Option, Sell or Close at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker
  Buy Put for Same Ticker
  Sell Put for Same Ticker

Buy Put Option, Sell or Close at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker
  Buy Put for Same Ticker   ✔
  Sell Put for Same Ticker

Sell Stock Short then Buy to Cover at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker ✔
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker ✔
  Buy Put for Same Ticker   ✔
  Sell Put for Same Ticker  ✔

Sell Call (Writer) then Close at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker ✔
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker ✔
  Buy Put for Same Ticker   ✔
  Sell Put for Same Ticker  ✔

Sell Put (Writer) then Close at a Loss

  Within 30 days you:       Wash Sale:
  ------------------------- ----------
  Buy Stock on Same Ticker  ✔
  Sell Stock on Same Ticker ✔
  Buy Call for Same Ticker  ✔
  Sell Call for Same Ticker ✔
  Buy Put for Same Ticker   ✔
  Sell Put for Same Ticker  ✔

-}

module ThinkOrSwim.Wash where

-- import Control.Lens
import Control.Monad.State
-- import Data.Amount
-- import Data.Ledger as Ledger
-- import Data.Time
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import ThinkOrSwim.Types

-- A losing closing transaction. If it closes within 31 days of open (30 + day
-- of sale), we record it as subject to the wash sale rule should a future
-- opening of the same position occur within the next 30 days. There's no need
-- to record profitable closing transactions.
{-
recordLoss :: UTCTime
           -> LotAndPL API.Transaction
           -> State (GainsKeeperState API.Transaction) ()
recordLoss w (LotAndPL pl l)
    -- If called for a profitable close, ignore.
    | pl <= 0 = pure ()
    -- | Just d <- l^.purchaseDate, w `diffUTCTime` d > 31 * 86400 = pure ()
    | otherwise =
      at (l^.Ledger.symbol).non (newEventHistory []).positionEvents
          <>= [TransactionEvent (Just pl) w l]
-}

-- Given a history of opening and closing transactions (and the opening
-- transaction for each of those), and a new opening transaction, determine
-- the washed losses, and the parts of the incoming transaction they apply to.
-- Any remainder is returned with a wash loss of 0.0. - The revised history,
-- with the washed transactions removed.
washSaleRule
    :: [LotAndPL API.Transaction]
    -> State (GainsKeeperState API.Transaction) [LotAndPL API.Transaction]
washSaleRule ls
{-
    | Just d <- l^.purchaseDate = pure []
      -- We're opening a transaction to which the wash sale rule may apply.
      -- Check whether an applicable losing transaction was made within the
      -- last 30 days, and if so, adjust the cost basis and remove the losing
      -- historical transaction since wash sales are only applied once.
      findApplicableClose l >>= \case
          Nothing -> do
              at (l^.Ledger.symbol).non (newEventHistory []).positionEvents
                  <>= [TransactionEvent Nothing d l]
              pure [l]
          Just _cl ->
              -- jww (2020-04-14): Transfer the loss on applicable share into
              -- the cost basis, possible splitting this opening transaction
              -- into multiple based on the number of losing shares. Then
              -- remove those shares from the historical record so they aren't
              -- applied again.
              pure [l]
-}
    | otherwise = pure ls
  where
    _findApplicableClose _ = pure Nothing

