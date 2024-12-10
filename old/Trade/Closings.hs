{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module Trade.Closings where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.State
import Data.Data
import Data.Foldable
import Data.IntMap (IntMap)
import Data.List (intersperse, sortOn)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics hiding (to)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show.Pretty hiding (Time)
import Trade.Journal.Entry
import Trade.Journal.Parse
import Trade.Journal.Print
import Trade.Journal.Split
import Trade.Journal.Types
import Prelude hiding (Double, Float)

data Disposition = Long | Short
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic, Data)

data Position = Position
  { _posIdent :: !Int,
    _posLot :: !Lot,
    _posDisp :: !Disposition
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Data,
      PrettyVal
    )

data Closing = Closing
  { _closingPos :: !Int,
    _closingLot :: !Lot
  }
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Data,
      PrettyVal
    )

makeLenses ''Position
makeLenses ''Closing

data PositionEvent
  = Open !Position -- open a position in the account
  | Close !Closing -- close a position in the account
  deriving
    ( Show,
      PrettyVal,
      Eq,
      Generic,
      Data
    )

makePrisms ''PositionEvent

data Calculation a = FIFO | LIFO | forall b. Ord b => Custom !(a -> b)

instance Show (Calculation a) where
  show FIFO = "FIFO"
  show LIFO = "LIFO"
  show (Custom _) = "Custom"

data BasicState a m = BasicState
  { _calc :: !(Calculation a),
    _nextId :: !Int,
    _events :: !m
  }
  deriving (Generic, Functor)

makeLenses ''BasicState

type ClosingState a = BasicState a (Map Text (IntMap a))

newClosingState :: Calculation a -> ClosingState a
newClosingState c =
  BasicState
    { _calc = c,
      _nextId = 0,
      _events = mempty
    }

type LocalState a = BasicState a (IntMap a)

localState :: Text -> Traversal' (ClosingState a) (LocalState a)
localState instrument f s =
  f (view (at instrument . non' _Empty) <$> s)
    <&> \m ->
      s
        & calc .~ (m ^. calc)
        & nextId .~ (m ^. nextId)
        & events . at instrument ?~ (m ^. events)

-- | This function returns the position events related to each incoming trade,
--   and also returns the set of open positions at the conclusion of all those
--   trades.
closings ::
  Calculation (Annotated PositionEvent) ->
  Fold a Trade ->
  [Annotated a] ->
  ( [[Annotated PositionEvent]],
    Map Text (IntMap (Annotated PositionEvent))
  )
closings mode p =
  second _events
    . flip runState (newClosingState mode)
    . mapM go
  where
    go x = case x ^? item . p of
      Just trade -> closing (trade <$ x)
      Nothing -> pure []

closing ::
  Annotated Trade ->
  State
    (ClosingState (Annotated PositionEvent))
    [Annotated PositionEvent]
closing entry = do
  gst <- get
  case entry ^? item . tradeLot . symbol of
    Just sym -> do
      let (results, gst') =
            flip runState gst $
              zoom (localState sym) $
                untilDone handle entry
      put gst'
      pure results
    Nothing -> pure []

handle ::
  Annotated Trade ->
  State
    (LocalState (Annotated PositionEvent))
    ( [Annotated PositionEvent],
      Remainder (Annotated Trade)
    )
handle ann@(preview item -> Just trade) = do
  mode <- use calc
  -- jww (2021-12-04): the buy/sell should be able to specify FIFO or LIFO,
  -- and the user should be able to set it as a default. In the case of LIFE,
  -- this traversal needs to be reversed.
  gets
    ( ( case mode of
          FIFO -> id
          LIFO -> reverse
          Custom f -> sortOn f
      )
        . (^.. events . traverse)
    )
    >>= \case
      open@(preview (item . _Open . posDisp) -> Just disp) : _
        | disp
            == case trade ^?! tradeAction of
              Sell -> Long
              Buy -> Short -> do
            events . at (open ^?! item . _Open . posIdent) .= Nothing
            closePosition open ann
      _ -> (, Finished) . (: []) <$> openPosition ann
handle _ = pure ([], Finished)

-- | Open a new position.
openPosition ::
  Annotated Trade ->
  State
    (LocalState (Annotated PositionEvent))
    (Annotated PositionEvent)
openPosition open = do
  nextId += 1
  ident <- use nextId
  let trade = open ^?! item
      event =
        _Open
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
  pure event

-- | Close an existing position. If the amount to close is more than what is
--   open, the remainder is considered a separate opening event.
closePosition ::
  Annotated PositionEvent ->
  Annotated Trade ->
  State
    (LocalState (Annotated PositionEvent))
    ( [Annotated PositionEvent],
      Remainder (Annotated Trade)
    )
closePosition open close =
  let o = open ^?! item . _Open
   in alignForClose
        (o ^. posLot)
        (close ^?! item . tradeLot)
        ( \_su du ->
            pure
              [ _Close
                  # Closing
                    { _closingPos = o ^. posIdent,
                      _closingLot = du
                    }
                  <$ close
              ]
        )
        ( \sk ->
            events
              . at (open ^?! item . _Open . posIdent)
              ?= (open & item . _Open . posLot .~ sk)
        )
        (\dk -> pure $ close & item . tradeLot .~ dk)

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
    <&> fromMaybe [] *** eitherRemainder

positions ::
  [Annotated PositionEvent] ->
  Map Text (IntMap (Annotated PositionEvent))
positions = foldl' positionsFromEvent mempty

positionsFromEvent ::
  Map Text (IntMap (Annotated PositionEvent)) ->
  Annotated PositionEvent ->
  Map Text (IntMap (Annotated PositionEvent))
positionsFromEvent m = go
  where
    go o@((^? item . _Open) -> Just pos) =
      m
        & at (pos ^. posLot . symbol)
          . non mempty
          . at (pos ^. posIdent)
          ?~ o
    go ((^? item . _Close) -> Just cl) =
      flip execState m do
        let loc ::
              Traversal'
                (Map Text (IntMap (Annotated PositionEvent)))
                (Maybe (Annotated PositionEvent))
            loc =
              at (cl ^. closingLot . symbol)
                . non mempty
                . at (cl ^. closingPos)
        preuse (loc . _Just) >>= \case
          Nothing ->
            error $
              "Attempt to close non-open position:\n"
                ++ ppShow cl
                ++ "\n---- against open positions ----\n"
                ++ ppShow m
          Just o ->
            alignedA
              (o ^?! item . _Open . posLot)
              (cl ^. closingLot)
              (\_ou _cu -> loc .= Nothing)
              (\ok -> loc ?= (o & item . _Open . posLot .~ ok))
              (\_ck -> error "Invalid closing")
    go _ = m

instance Printable PositionEvent where
  printItem = printEvent

instance HasLot PositionEvent where
  _Lot f (Open s) = Open <$> (s & posLot %%~ f)
  _Lot f (Close s) = Close <$> (s & closingLot %%~ f)

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
      [ TL.pack (show _closingPos),
        printLot _closingLot
      ]

parsePosition :: Parser Position
parsePosition =
  Position
    <$> L.decimal
    <* whiteSpace
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
