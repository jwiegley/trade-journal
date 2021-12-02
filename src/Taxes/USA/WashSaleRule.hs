{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Taxes.USA.WashSaleRule where

import Amount
import Control.Exception (assert)
import Control.Lens
import Control.Monad
import Data.Functor
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time
import Debug.Trace
import GHC.Generics
import Journal.Parse
import Journal.Print
import Journal.Split
import Journal.Types
import Journal.Utils (distance)
import Journal.Zippered
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Show.Pretty hiding (Time)

data Period = Past | Future
  deriving (Show, PrettyVal, Eq, Ord, Enum, Bounded, Generic)

data Washing
  = Exempt
  | WashTo Text (Maybe (Amount 6, Amount 6))
  | Wash Period (Amount 6) Int
  | WashApply Text (Amount 6) -- per share wash applied
  | WashedFrom Period (Amount 6) Lot
  deriving (Show, PrettyVal, Eq, Ord, Generic)

makePrisms ''Washing

printWashing :: Washing -> Either TL.Text TL.Text
printWashing = \case
  WashedFrom Past x _ -> Right $ "washed from past " <> printAmount 2 x
  WashedFrom Future x _ -> Right $ "washed from future " <> printAmount 2 x
  WashTo x (Just (q, p)) ->
    Left $
      "wash "
        <> printAmount 0 q
        <> " @ "
        <> printAmount 4 p
        <> " to "
        <> TL.fromStrict x
  WashTo x Nothing -> Left $ "wash to " <> TL.fromStrict x
  WashApply x amt ->
    Left $ "apply " <> TL.fromStrict x <> " " <> printAmount 0 amt
  Exempt -> Left "exempt"

parseWashing :: Parser Washing
parseWashing =
  -- keyword "wash"
  --   *> keyword "from"
  --   *> keyword "past"
  --   *> (WashedFromPast <$> parseTime <*> parseLot)
  --   <|> keyword "wash"
  --     *> keyword "from"
  --     *> keyword "future"
  --     *> (WashedFromFuture <$> parseTime <*> parseLot)
  -- <|> keyword "washed" *> (Washed <$> parseAmount)
  -- <|> keyword "wash" *> parseWash
  keyword "apply"
    *> (WashApply . TL.toStrict <$> parseSymbol <*> parseAmount)
    <|> (keyword "exempt" $> Exempt)
  where
    parseWash = do
      mres <- optional $ do
        q <- parseAmount
        _ <- char '@' <* whiteSpace
        p <- parseAmount
        pure (q, p)
      _ <- keyword "to"
      sym <- parseSymbol
      pure $ WashTo (TL.toStrict sym) mres

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule :: [Annotated (Entry [Washing])] -> [Annotated (Entry [Washing])]
washSaleRule =
  map snd
    . go
    . map
      ( \x ->
          ( null (x ^.. opening . posData . traverse . _Exempt)
              && null (x ^.. closing . closingData . traverse . _Exempt),
            x
          )
      )
  where
    go ::
      [(Bool, Annotated (Entry [Washing]))] ->
      [(Bool, Annotated (Entry [Washing]))]
    go xs = maybe xs go $ do
      -- The wash sale rule proceeds by looking for losing sales that haven't
      -- been washed. If there are none, we are done.
      let z = eligibleClose xs
      x <- z ^? focus
      c <- x ^? _2 . closing
      traceM $ "c = " ++ ppShow c

      -- Once an eligible losing sale is found, we look for an eligible
      -- opening within 30 days before or after that sale.
      (z', xx') <-
        applyToPrefixOrSuffix (eligibleOpen (x ^. _2 . time)) (handleOpen x) z

      let z'' = z' & focii .~ xx'
      guard $ z /= z''
      pure $ unzippered z''

    gains :: Closing [Washing] -> Amount 6
    gains c =
      let p = c ^. closingLot . price
          b = c ^. closingPos . posBasis
       in case c ^. closingPos . posDisp of
            Long -> p - b
            Short -> b - p

    eligibleClose ::
      [(Bool, Annotated (Entry [Washing]))] ->
      Zippered (Bool, Annotated (Entry [Washing]))
    eligibleClose = zippered $ \(w, x) ->
      w && case x ^? closing of
        Just c | gains c < 0 -> True
        _ -> False

    eligibleOpen anchor (w, y) =
      w && has opening y && abs (anchor `distance` (y ^. time)) <= 30

    handleOpen ::
      (Bool, Annotated (Entry [Washing])) ->
      Bool ->
      Zippered (Bool, Annotated (Entry [Washing])) ->
      Maybe
        ( Zippered (Bool, Annotated (Entry [Washing])),
          [(Bool, Annotated (Entry [Washing]))]
        )
    handleOpen x inPast part = do
      c <- x ^? _2 . closing
      let loss = gains c
      assert (loss < 0) $ do
        y <- part ^? focus
        o <- y ^? _2 . opening
        traceM $ "o = " ++ ppShow o
        traceM $ "loss = " ++ ppShow loss

        (Just (l, r), reyz) <-
          alignedA
            (c ^. closingLot)
            (o ^. posLot)
            (curry pure)
            pure
            pure

        let o' =
              o & posBasis -~ loss
                & posLot .~ r
                & posData
                  <>~ [ WashedFrom
                          ( if inPast
                              then Future
                              else Past
                          )
                          loss
                          (c ^. closingLot)
                      ]
            c' =
              c & closingLot .~ l
                & closingData
                  <>~ [ Wash
                          ( if inPast
                              then Past
                              else Future
                          )
                          loss
                          (o' ^. posIdent)
                      ]
        traceM $ "o' = " ++ ppShow o'
        traceM $ "c' = " ++ ppShow c'
        pure
          ( part & focii
              .~ ( ( y & _1 .~ False
                       & _2 . opening .~ o'
                   ) :
                   case reyz of
                     Remainder (Right z) ->
                       [ y & _1 .~ False
                           & _2 . opening . posLot .~ z
                       ]
                     _ -> []
                 ),
            ( x & _1 .~ False
                & _2 . closing .~ c'
            ) :
            case reyz of
              Remainder (Left z) ->
                [x & _2 . closing . closingLot .~ z]
              _ -> []
          )

testWashSale :: IO ()
testWashSale = do
  now <- getCurrentTime
  putStrLn $
    dumpStr $
      washSaleRule (map (\x -> Annotated x now []) dataset)
  where
    dataset :: [Entry [Washing]]
    dataset =
      [ Event
          ( Open
              ( Position
                  1
                  Lot
                    { _amount = 100,
                      _symbol = "FOO",
                      _price = 100
                    }
                  Long
                  100
                  []
              )
          ),
        Event
          ( Open
              ( Position
                  2
                  Lot
                    { _amount = 200,
                      _symbol = "FOO",
                      _price = 200
                    }
                  Long
                  200
                  []
              )
          ),
        Event
          ( Open
              ( Position
                  3
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
                  Long
                  300
                  []
              )
          ),
        Event
          ( Close
              ( Closing
                  ( Position
                      1
                      Lot
                        { _amount = 100,
                          _symbol = "FOO",
                          _price = 100
                        }
                      Long
                      100
                      []
                  )
                  Lot
                    { _amount = 100,
                      _symbol = "FOO",
                      _price = 50
                    }
                  []
              )
          ),
        Event
          ( Open
              ( Position
                  4
                  Lot
                    { _amount = 200,
                      _symbol = "FOO",
                      _price = 200
                    }
                  Long
                  200
                  []
              )
          ),
        Event
          ( Open
              ( Position
                  5
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
                  Long
                  300
                  []
              )
          ),
        Event
          ( Close
              ( Closing
                  ( Position
                      2
                      Lot
                        { _amount = 200,
                          _symbol = "FOO",
                          _price = 200
                        }
                      Long
                      200
                      []
                  )
                  Lot
                    { _amount = 300,
                      _symbol = "FOO",
                      _price = 300
                    }
                  []
              )
          )
      ]
