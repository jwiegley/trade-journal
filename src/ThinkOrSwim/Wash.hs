{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module ThinkOrSwim.Wash
    ( GainsKeeperState(..)
    , newGainsKeeperState
    , openTransactions
    , SymbolHistory
    , symbolHistory
    , history
    , washSaleRule
    , Entry(..)
    , AnEntry(..)
    ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Lens
import Control.Monad.State
import Data.Coerce
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Split
import Data.Time
import Data.Utils
import Prelude hiding (Float, Double, (<>))
import Text.Show.Pretty
import Text.PrettyPrint as P
import ThinkOrSwim.Transaction

-- Trading events that bear upon the wash sale rule.
data Event
    = Open
    | Close
    | LosingClose
    deriving (Eq, Ord, Show, Enum, Bounded)

type EntryId = Int64

data Entry (k :: Event) a = Entry
    { _entId   :: EntryId        -- uniquely identifies each entry
    , _entItem :: a
    }
    deriving (Eq, Ord)

instance Transactional a => Show (Entry k a) where
    show Entry {..} = "#" ++ show _entId ++ " :: " ++ showPretty _entItem

data AnEntry a where
    Opener       :: Entry Open a -> AnEntry a
    Closer       :: Entry Close a -> AnEntry a
    LosingCloser :: Entry LosingClose a -> AnEntry a

deriving instance Eq a => Eq (AnEntry a)
deriving instance Ord a => Ord (AnEntry a)
deriving instance Transactional a => Show (AnEntry a)

overEntry :: AnEntry a -> (forall k. Entry k a -> Entry k a) -> AnEntry a
overEntry (Opener e) k       = Opener (k e)
overEntry (Closer e) k       = Closer (k e)
overEntry (LosingCloser e) k = LosingCloser (k e)

-- After a trading event is matched with its history, the series of resulting
-- matches cause a corresponding series of modifications to the history or the
-- trade itself.
data Action a
    = AddOpen (Entry Open a)
    | AddLosingClose (Entry LosingClose a)
    | RemoveOpen (Entry Open a)
    | RemoveLosingClose (Entry LosingClose a)
    | AdjustCostBasis (Entry Open a) (Entry LosingClose a)
    | WashOpen (Entry LosingClose a)

deriving instance Transactional a => Show (Action a)

makePrisms ''Event
makeLenses ''Entry
makePrisms ''AnEntry
makePrisms ''Action

anEntryItem :: Lens' (AnEntry a) a
anEntryItem f = \case
    Opener e       -> Opener <$> (e & entItem %%~ f)
    Closer e       -> Closer <$> (e & entItem %%~ f)
    LosingCloser e -> LosingCloser <$> (e & entItem %%~ f)

data GainsKeeperState t e = GainsKeeperState
    { _openTransactions :: Map Text [t]
    , _positionEvents   :: Map Text [AnEntry e]
    , _nextEventId      :: EntryId
    }
    deriving (Eq, Ord, Show)

makeLenses ''GainsKeeperState

newGainsKeeperState :: GainsKeeperState t e
newGainsKeeperState = GainsKeeperState
    { _openTransactions = mempty
    , _positionEvents   = mempty
    , _nextEventId      = 1
    }

data SymbolHistory t a = SymbolHistory
    { _xacts   :: [t]
    , _history :: [AnEntry a]
    , _nextId  :: EntryId
    }

makeLenses ''SymbolHistory

symbolHistory :: (Eq t, Eq a)
              => Text -> Lens' (GainsKeeperState t a) (SymbolHistory t a)
symbolHistory sym f s =
    f (SymbolHistory (s^.openTransactions.at sym.non [])
                     (s^.positionEvents.at sym.non [])
                     (s^.nextEventId)) <&> \h ->
      s & openTransactions.at sym ?~ h^.xacts
        & positionEvents.at sym   ?~ h^.history
        & nextEventId             .~ h^.nextId

instance forall k a. Transactional a => Transactional (Entry k a) where
    symbol            = entItem.symbol
    quantity          = entItem.quantity
    cost              = entItem.cost
    price             = entItem.price
    day               = entItem.day
    loss              = entItem.loss
    ident             = entItem.ident
    washDeferred      = entItem.washDeferred
    washEligible      = entItem.washEligible
    washLoss b x y    = y & entItem %~ washLoss b x
    clearLoss x       = x & entItem %~ clearLoss
    isTransferIn x    = isTransferIn (x^.entItem)
    arePaired     x y = (x^.entItem) `arePaired` (y^.entItem)
    areEquivalent x y = (x^.entItem) `areEquivalent` (y^.entItem)
    showPretty        = showPretty . view entItem

_Entry :: Transactional a => EntryId -> Prism' (AnEntry a) a
_Entry next = prism' build fold
  where
    build t
        | t^.quantity > 0 = assert (t^.loss == 0) $
            Opener Entry { _entId   = next
                         , _entItem = t }
        | t^.loss <= 0 =
            Closer Entry { _entId   = next
                         , _entItem = t }
        | otherwise =
            LosingCloser Entry { _entId   = next
                               , _entItem = t }
    fold = \case
        Opener e       -> Just (e^.entItem)
        Closer e       -> Just (e^.entItem)
        LosingCloser e -> Just (e^.entItem)

newEntry :: Transactional a => a -> State (SymbolHistory t a) (AnEntry a)
newEntry t = do
    next <- use nextId
    nextId += 1
    pure $ _Entry next # t

findMatches :: (Transactional a, Show a)
            => [AnEntry a] -> AnEntry a -> [Action a]
findMatches = go
  where
    go [] (Opener y) = [AddOpen y]
    go [] _ = []

    go (Opener x:xs) (Closer y)
        | Just u <- s^?_SplitUsed = assert (has _SplitUsed d) $
            RemoveOpen u :
            continue d s Opener Closer xs
      where
        (s, d) = x `alignLots` y

    go (Opener x:xs) (LosingCloser y)
        | Just us <- s^?_SplitUsed,
          Just ud <- d^?_SplitUsed =
            RemoveOpen us :
            -- Is there an opening after this that can be adjusted now? Otherwise,
            -- recall the loss for later washing.
            AddLosingClose ud :
            continue d s Opener LosingCloser xs
      where
        (s, d) = x `alignLots` y

    go (Opener x:xs) y =
        AddOpen x : go xs y

    go (LosingCloser x:xs) (Opener y)
        | Just us <- s^?_SplitUsed,
          Just ud <- d^?_SplitUsed=
            RemoveLosingClose us :
            AddOpen (ud & cost +~ coerce (us^.loss)) :
            WashOpen us :
            continue d s LosingCloser Opener xs
      where
        (s, d) = x `alignLots` y

    go (LosingCloser x:xs) y =
        AddLosingClose x : go xs y

    go xs _ = error $ "Invalid history: " ++ ppShow xs

    continue :: (Transactional a, Show a)
             => Split b
             -> Split c
             -> (c -> AnEntry a)
             -> (b -> AnEntry a)
             -> [AnEntry a]
             -> [Action a]
    continue d s f g xs = case d^?_SplitKept of
        Nothing -> []
        Just y' -> go (case s^?_SplitKept of
                          Nothing -> xs
                          Just k -> f k:xs)
                     (g y')

evalActions :: Transactional a
            => AnEntry a -> [Action a]
            -> State (SymbolHistory t a) (AnEntry a, [AnEntry a])
evalActions ent = go
  where
    go [] = pure (ent, [])
    go (x:xs) = do
        res@(r, rest) <- go xs
        case x of
            AddOpen o ->
                res <$ (history %= (Opener o:))
            AddLosingClose lc ->
                res <$ (history %= (LosingCloser lc:))
            RemoveOpen o ->
                res <$ (history %= filter (not . openMatches o))
            RemoveLosingClose lc ->
                res <$ (history %= filter (not . closeMatches lc))
            AdjustCostBasis o lc -> do
                history <>= []
                pure (r, Closer (coerce o & quantity %~ negate)
                           : Opener (washLoss True lc o)
                           : rest)
            WashOpen lc ->
                pure (overEntry r (washLoss True lc), rest)
      where
        openMatches i (Opener j) = i^.entId == j^.entId
        openMatches _ _ = False

        closeMatches i (LosingCloser j) = i^.entId == j^.entId
        closeMatches _ _ = False

-- This function assumes 'l' is received in temporal order. For this reason,
-- we remove all entries older than 30 days from the list before beginning.
--
-- If 'l' is an opening transaction:
--
--   a. No past losses apply, add it to the history, return it.
--   b. A past loss (1) applies. Reprice the open, remove (1), add the
--      repriced transaction to the history and return it.
--
-- If 'l' is a closing transaction:
--
--   a. No past openings apply, just return it.
--   b. A past opening (1) applies. Add it to the history, remove (1), and
--      return the transaction.
--   c. A past opening (1) applies and there is another past opening (2) to
--      which the loss applies. Remove (1) and (2), generate a sale/repurchase
--      of (2), remove (1). Add the repriced (2) back to the history and
--      return the input transaction with its loss removed along with (2).
--
-- Please see the tests for more comprehensive examples. The principal
-- scenarios that require a wash sale adjustment are:
--
-- - open, close <30(o), open <30(c)
-- - open, open <30(c), close <30(o)

washSaleRule :: (Transactional a, Show a, Transactional t, Eq t, Show t)
             => Lens' a t -> Traversal' a Day -> a
             -> State (SymbolHistory t a) (Doc, [a])
washSaleRule _ablens _dy l = do
    ent <- newEntry l
    hs  <- use history
    let actions = findMatches hs ent
    (e, es) <- evalActions ent actions
    hs' <- use history
    let doc = P.empty
            $$ text "pl      = " <> text (showPretty l)
            $$ text "before  = " <> renderList (text . ppShow) hs
            $$ text "raw     = " <> text (ppShow ent)
            $$ text "actions = " <> renderList (text . ppShow) actions
            $$ text "ent     = " <> text (ppShow e)
            $$ text "ents    = " <> renderList (text . ppShow) es
            $$ text "after   = " <> renderList (text . ppShow) hs'
    pure (doc, (e:es)^..traverse.anEntryItem)

{-
    | l^.loss == 0 = do

      events <- use history
      let c   = matchEvents events l id (const True)
          -- res = c^..consideredElements
          l'  = foldl' (flip (washLoss True)) l (c^.fromList)
      -- history .= c^.newList ++ map clearLoss res
      history .= c^.newList ++ [clearLoss l']
      let doc = "\nhave opening xact  : " <> text (showPretty l)
             $$ "current history    : " <> renderTransactions events
             $$ "remove from history: " <> renderTransactions (c^.fromList)
             $$ "put back in history: " <> renderTransactions (c^.newList)
             $$ "add to history     : " <> renderTransactions [clearLoss l']
             -- $$ "add to history     : " <> renderTransactions (map clearLoss res)
             $$ "return result      : " <> renderTransactions [l']
             -- $$ "return result      : " <> renderTransactions res
      -- pure (doc, res)
      pure (doc, [l'])

    | l^.loss > 0 = do

      events <- use history
      let doc = "\nhave losing xact   : " <> text (showPretty l)
             $$ "current history    : " <> renderTransactions events
      let c = matchEvents events l id (const True)
      case c^.fromElement of
          [] -> do
              history .= c^.newList
              let doc' = doc
                      $$ "remove from history: " <> renderTransactions (c^.fromList)
                      $$ "return result      : " <> renderTransactions [l]
              pure (doc', [l])
          xs -> do
              (doc', ys, zs, nl) <- foldM retroact (doc, [], [], c^.newList) xs
              history .= map clearLoss zs ++ nl
              let doc'' = doc'
                      $$ "considered         : " <> renderConsidered c
                      $$ "put back in history: " <> renderTransactions nl
                      $$ "add to history     : " <> renderTransactions (map clearLoss zs)
                      $$ "return result      : " <> renderTransactions (l:intermix ys zs)
                  rm = l^.loss + sumOf (each.loss) zs
                  l' = l & washDeferred .~ if rm /= 0 then Just rm else Nothing
              pure (doc'', l':intermix ys zs)

    | otherwise = do

      events <- use history
      let c = matchEvents events l id (const True)
      history .= c^.newList
      let doc = "\nhave winning xact: " <> text (showPretty l)
             $$ "current history    : " <> renderTransactions events
             $$ "remove from history: " <> renderTransactions (c^.fromList)
             $$ "put back in history: " <> renderTransactions (c^.newList)
             $$ "return result      : " <> renderTransactions [l]
      pure (doc, [l])

  where
    -- wash = zipped (src._SplitUsed) (dest._SplitUsed)
    --     %~ \(x, y) -> (x, washLoss False x y)

    intermix [] [] = []
    intermix (x:xs) (y:ys) = x:y:intermix xs ys
    intermix xs ys = error $ "intermix called with uneven lengths: "
        ++ showTransactions xs ++ " and " ++ showTransactions ys

    retroact (doc, ys', zs', nl') x = do
        fel <- (\f -> zipWithM f
                      (d^.fromElement)
                      (d^.fromList)) $ \e i -> do
            let j = washLoss False e i & dy .~ l^.day
            _1.traverse %= \y ->
                if y == i^.ablens then j^.ablens else y
            pure j
        pure ( doc $$ "ys': " <> renderTransactions ys'
                   $$ "zs': " <> renderTransactions zs'
                   $$ "nl': " <> renderTransactions nl'
             , ys' ++ (d^.fromList & each.quantity %~ negate)
             , zs' ++ fel
             , d^..consideredNew )
      where
        d = matchEvents nl' x id (view washEligible)

-- Given a list of transactional elements, and some candidate, find all part, Show bs
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
            => [a] -> a -> (Applied () a a -> Applied () a a) -> (a -> Bool)
            -> Considered a a a
matchEvents hs pl k p =
    consider f (\_ () -> id) (filter (within 30 pl) hs) pl
  where
    within n x y = abs ((x^.day) `diffDays` (y^.day)) <= n

    f h x | areEquivalent h x && (opening || closing) && p h =
              k (splitLots h x)
          | otherwise = nothingApplied @() h x
      where
        closing = h^.loss == 0 && x^.loss >  0
        opening = h^.loss >  0 && x^.loss == 0
-}

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
