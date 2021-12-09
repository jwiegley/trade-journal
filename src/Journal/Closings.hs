{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Journal.Closings where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Aeson hiding ((.=))
import Data.Foldable
import Data.Functor.Classes
import Data.IntMap (IntMap)
import Data.List (intersperse)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Sum
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import Journal.Entry
import Journal.Entry.Trade
import Journal.Parse
import Journal.Print
import Journal.Split
import Data.Sum.Lens
import Journal.Types
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show.Pretty hiding (Time)
import Prelude hiding (Double, Float)

data Disposition = Long | Short
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic)

data Position = Position
  { _posIdent :: Int,
    _posLot :: Lot,
    _posDisp :: Disposition
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

data Closing = Closing
  { _closingIdent :: Int,
    _closingLot :: Lot
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal
    )

makeLenses ''Position
makeLenses ''Closing

-- instance Splittable (Amount 6) Position where
--   howmuch = posLot . amount

-- instance Splittable (Amount 6) Closing where
--   howmuch = closingLot . amount

data PositionEvent
  = Open Position -- open a position in the account
  | Close Closing -- close a position in the account
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic
    )

makePrisms ''PositionEvent

class HasPositionEvent f where
  _Event :: Traversal' (f v) PositionEvent

instance HasTraversal' HasPositionEvent fs => HasPositionEvent (Sum fs) where
  _Event = traversing @HasPositionEvent _Event

instance HasPositionEvent (Const Entry) where
  _Event _ = pure

instance HasPositionEvent (Const Trade) where
  _Event _ = pure

instance HasPositionEvent (Const PositionEvent) where
  _Event f (Const s) = Const <$> f s

_EventLot :: Traversal' PositionEvent Lot
_EventLot f = \case
  Open pos -> Open <$> (pos & posLot %%~ f)
  Close cl -> Close <$> (cl & closingLot %%~ f)

instance HasLot (Const PositionEvent) where
  _Lot f (Const s) = fmap Const $ s & _EventLot %%~ f

data Calculation = FIFO | LIFO
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      PrettyVal,
      FromJSON
    )

data BasicState e = BasicState
  { _calc :: Calculation,
    _nextId :: Int,
    _events :: e
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Functor,
      Foldable,
      Traversable
    )

makeLenses ''BasicState

type ClosingState r v = BasicState (Map Text (IntMap (Annotated (Sum r v))))

newClosingState :: Calculation -> ClosingState r v
newClosingState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState r v = BasicState (IntMap (Annotated (Sum r v)))

localState ::
  Text ->
  Traversal' (ClosingState r v) (LocalState r v)
localState instrument f s =
  f (view (at instrument . non' _Empty) <$> s) <&> \m ->
    s & nextId .~ (m ^. nextId)
      & events . at instrument ?~ (m ^. events)

closings ::
  Const Trade :< r =>
  Calculation ->
  [Annotated (Sum r v)] ->
  ( [Annotated (Sum (Const PositionEvent ': r) v)],
    Map Text (IntMap (Annotated (Sum (Const PositionEvent ': r) v)))
  )
closings mode =
  (concat *** _events) . flip runState (newClosingState mode) . mapM go
  where
    go entry = do
      gst <- get
      case entry ^? item . projectedC . tradeLot . symbol of
        Just sym -> do
          let (results, gst') =
                flip runState gst $
                  zoom (localState sym) $
                    untilDone handle entry
          put gst'
          pure (fmap weaken entry : results)
        Nothing -> pure [fmap weaken entry]

handle ::
  Const Trade :< r =>
  Annotated (Sum r v) ->
  State
    (LocalState (Const PositionEvent ': r) v)
    ( [Annotated (Sum (Const PositionEvent ': r) v)],
      Remainder (Annotated (Sum r v))
    )
handle ann@(preview (item . projectedC) -> Just trade) = do
  mode <- use calc
  -- jww (2021-12-04): the buy/sell should be able to specify FIFO or LIFO,
  -- and the user should be able to set it as a default. In the case of LIFE,
  -- this traversal needs to be reversed.
  gets
    ( (case mode of FIFO -> id; LIFO -> reverse)
        . (^.. events . traverse)
    )
    >>= \case
      open@(preview (item . projectedC . _Open . posDisp) -> Just disp) : _
        | disp
            == case trade ^?! tradeAction of
              Sell -> Long
              Buy -> Short -> do
          events . at (open ^?! item . projectedC . _Open . posIdent) .= Nothing
          closePosition open ann
      _ -> (,Finished) <$> openPosition ann
handle _ = pure ([], Finished)

-- | Open a new position.
openPosition ::
  Const Trade :< r =>
  Annotated (Sum r v) ->
  State
    (LocalState (Const PositionEvent ': r) v)
    [Annotated (Sum (Const PositionEvent ': r) v)]
openPosition open = do
  nextId += 1
  ident <- use nextId
  let trade = open ^?! item . projectedC
      event =
        projectedC . _Open
          # Position
            { _posIdent = ident,
              _posLot = trade ^. tradeLot,
              _posDisp =
                case trade ^?! tradeAction of
                  Buy -> Long
                  Sell -> Short
            }
          <$ open
  events . at ident ?= event
  pure [event]

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  Const Trade :< r =>
  Annotated (Sum (Const PositionEvent ': r) v) ->
  Annotated (Sum r v) ->
  State
    (LocalState (Const PositionEvent ': r) v)
    ( [Annotated (Sum (Const PositionEvent ': r) v)],
      Remainder (Annotated (Sum r v))
    )
closePosition open close =
  let o = open ^?! item . projectedC . _Open
   in alignForClose
        (o ^. posLot)
        (close ^?! item . projectedC . tradeLot)
        ( \_su du ->
            pure
              [ projectedC . _Close
                  # Closing
                    { _closingIdent = o ^. posIdent,
                      _closingLot = du
                    }
                  <$ close
              ]
        )
        ( \sk ->
            events . at (open ^?! item . projectedC . _Open . posIdent)
              ?= (open & item . projectedC . _Open . posLot .~ sk)
        )
        (\dk -> pure $ close & item . projectedC . tradeLot .~ dk)

alignForClose ::
  (Splittable n a, Splittable n b, Applicative m) =>
  a ->
  b ->
  (a -> b -> m [x]) ->
  (a -> m ()) ->
  (b -> m z) ->
  m ([x], Remainder z)
alignForClose l r f g h =
  alignedA l r f g h
    <&> fromMaybe []
      *** \case
        Remainder (Right r') -> Remainder r'
        _ -> Finished

positions ::
  ( HasTraversal' HasPositionEvent r,
    Apply Eq1 r,
    Eq v,
    Apply Show1 r,
    Show v
  ) =>
  [Annotated (Sum r v)] ->
  Map Text (IntMap (Annotated (Sum r v)))
positions = foldl' positionsFromEntry mempty

positionsFromEntry ::
  ( HasTraversal' HasPositionEvent r,
    Apply Eq1 r,
    Eq v,
    Apply Show1 r,
    Show v
  ) =>
  Map Text (IntMap (Annotated (Sum r v))) ->
  Annotated (Sum r v) ->
  Map Text (IntMap (Annotated (Sum r v)))
positionsFromEntry m = go
  where
    go o@((^? item . _Event . _Open) -> Just pos) =
      m
        & at (pos ^. posLot . symbol)
          . non mempty
          . at (pos ^. posIdent)
        ?~ o
    go ((^? item . _Event . _Close) -> Just cl) =
      flip execState m do
        let loc ::
              (Apply Eq1 r, Eq v) =>
              Traversal'
                (Map Text (IntMap (Annotated (Sum r v))))
                (Maybe (Annotated (Sum r v)))
            loc =
              at (cl ^. closingLot . symbol)
                . non mempty
                . at (cl ^. closingIdent)
        preuse (loc . _Just) >>= \case
          Nothing ->
            error $
              "Attempt to close non-open position:\n"
                ++ ppShow cl
                ++ "\n---- against open positions ----\n"
                ++ ppShow m
          Just o ->
            alignedA
              (o ^?! item . _Event . _Open . posLot)
              (cl ^. closingLot)
              (\_ou _cu -> loc .= Nothing)
              (\ok -> loc ?= (o & item . _Event . _Open . posLot .~ ok))
              (\_ck -> error "Invalid closing")
    go _ = m

instance Printable (Const PositionEvent) where
  printItem = printEvent . getConst

printEvent :: PositionEvent -> TL.Text
printEvent = \case
  Open pos -> printPosition pos
  Close cl -> printClosing cl

printPosition :: Position -> TL.Text
printPosition Position {..} =
  TL.pack (show _posIdent)
    <> " "
    <> printLot _posLot
    <> " "
    <> printDisposition _posDisp
  where
    printDisposition Long = "long"
    printDisposition Short = "short"

printClosing :: Closing -> TL.Text
printClosing Closing {..} =
  TL.concat $
    intersperse
      " "
      [ TL.pack (show _closingIdent),
        printLot _closingLot
      ]

parsePosition :: Parser Position
parsePosition =
  Position
    <$> L.decimal <* whiteSpace
    <*> parseLot
    <*> parseDisposition
  where
    parseDisposition :: Parser Disposition
    parseDisposition =
      Long <$ keyword "long"
        <|> Short <$ keyword "short"

-- jww (2021-12-03): A closing should display what it's closing the open
-- position to, for example: FOO 100 @ <basis> -> 50.
parseClosing :: Parser Closing
parseClosing =
  Closing
    <$> (L.decimal <* whiteSpace)
    <*> parseLot
