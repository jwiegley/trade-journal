{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module ThinkOrSwim.Fixup (fixupTransaction) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Amount
import qualified Data.Ledger as L
import           Prelude hiding (Float, Double, (<>))
import qualified ThinkOrSwim.API.TransactionHistory.GetTransactions as API
import           ThinkOrSwim.Transaction
import           ThinkOrSwim.Transaction.Instances ()
import           ThinkOrSwim.Types

fixupTransaction
    :: L.Transaction API.TransactionSubType API.Order
                    API.Transaction L.LotAndPL
    -> State (GainsKeeperState API.TransactionSubType API.Transaction)
            (L.Transaction API.TransactionSubType API.Order
                           API.Transaction L.LotAndPL)
fixupTransaction t | not xactOA = pure t
  where
    hasOA  = has (L.plLot.L.kind.API._OptionAssignment)
    xactOA = anyOf L.optionLots hasOA t
           && anyOf L.equityLots hasOA t

fixupTransaction t =
    t & L.optionLots.L.plLoss .~ 0
      & L.optionLots.L.plKind .~ L.BreakEven
      & partsOf L.equityLots %%~ mapM f . spreadAmounts (^.quantity) value
  where
    value = sumOf (L.optionLots.L.plLot.cost) t

    f (n, pl) = do
        let pl' = pl & cost -~ sign (pl^.quantity) n
        zoom (openTransactions.at (pl^.symbol)) $
            _Just.traverse %= \x ->
                if x == pl^.L.plLot then pl'^.L.plLot else x
        pure pl'
