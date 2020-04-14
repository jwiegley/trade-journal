{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
           |  up to 30 days
           v
    C. Buy <=B shares of "STCK equivalent"

    Example: Buy 100 shares, sell 50, buy 100: first 50 shares of those 100
    repurchased have their cost basis adjusted to include the loss from the
    sale of 50.

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


NOTE: The IRS has ruled (Rev. Rul. 2008-5) that when an individual sells a
security at a loss and then repurchases that security in their (or their
spouses’) IRA within 30 days before or after the sale, that loss will be
subject to the wash-sale rules.

-}

module ThinkOrSwim.Wash where

-- import Control.Lens
-- import Control.Monad.State
import Data.Amount
-- import Data.Coerce
-- import Data.Foldable (foldl')
import Data.Ledger as Ledger
-- import Data.Maybe (isJust, maybeToList)
import Data.Time
-- import GHC.TypeLits
import Prelude hiding (Float, Double)
import ThinkOrSwim.API.TransactionHistory.GetTransactions as API
-- import ThinkOrSwim.Types

-- import Data.List (intercalate)
-- import Data.Text (unpack)
-- import Debug.Trace

-- Given a history of closing transactions that incurred losses (and the
-- opening transaction for each), and a new opening transaction, determine:
--
-- - The washed losses, and the parts of the incoming transaciton they apply
--   to. Any remainder is returned with a wash loss of 0.0.
-- - The revised history, with the washed transactions removed.
washSaleRule
    :: [(Amount 2, UTCTime, CommodityLot API.Transaction)]
    -> CommodityLot API.Transaction
    -> ( [(Amount 2, CommodityLot API.Transaction)]
      , [(Amount 2, UTCTime, CommodityLot API.Transaction)]
      )
washSaleRule hist lot = ([(0.0, lot)], hist)
