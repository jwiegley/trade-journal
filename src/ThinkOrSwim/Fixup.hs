{-# LANGUAGE FlexibleContexts #-}

module ThinkOrSwim.Fixup (fixupTransaction) where

import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import           Data.Ledger hiding (symbol, quantity, cost)
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Transaction.Instances ()

-- If a transaction represents an options assignment, where the closing of a
-- short option has resulted in a forced purchase or sale, factor the premium
-- from the option sale into either the cost basis of the purchased shares, or
-- capital gain/loss of the sold shares -- taking into account the fact that
-- multiple lots of call option contracts may be closing, which may result in
-- the sale or purchase of multiple lots of equity.

fixupTransaction
    :: Transaction API.TransactionSubType API.Order LotAndPL
    -> State (GainsKeeperState API.TransactionSubType)
            (Transaction API.TransactionSubType API.Order LotAndPL)
fixupTransaction t | not xactOA = pure t
  where
    hasOA  = has (plLot.kind.API._OptionAssignment)
    xactOA = anyOf optionLots hasOA t
           && anyOf equityLots hasOA t

fixupTransaction t =
    t & optionLots.plLoss   .~ 0
      & optionLots.plKind   .~ BreakEven
      & partsOf equityLots %%~ mapM f . spreadAmounts (^.quantity) value
  where
    value = sumOf (optionLots.cost) t

    f (n, pl) = do
        let pl' = pl & cost -~ sign (pl^.quantity) n
        zoom (openTransactions.at (pl^.symbol)) $
            _Just.traverse %= \x ->
                if x == pl^.plLot then pl'^.plLot else x
        pure pl'
