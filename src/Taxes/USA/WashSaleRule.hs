{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Taxes.USA.WashSaleRule (washSaleRule) where

import Amount
import Control.Exception (assert)
import Control.Lens
import Control.Monad
import Data.Time
import Debug.Trace
import Journal.Split
import Journal.Types
import Journal.Utils (distance)
import Journal.Zippered
import Text.Show.Pretty hiding (Time)

opening :: Traversal' (Annotated Entry) Position
opening = item . _Event . _Open

closing :: Traversal' (Annotated Entry) Closing
closing = item . _Event . _Close

-- This implementation of the wash sale rule requires that we know the total
-- set of broker events in advance. Basically, each time we encounter a
-- non-exempt position open, we check 30 days back and then 30 days forward
-- for an eligible losing close; if it exists, the loss is moved into the cost
-- basis of the open.
washSaleRule ::
  [Annotated Entry] ->
  [Annotated Entry]
washSaleRule =
  map snd
    . go
    . map
      ( \x ->
          ( null (x ^.. opening . posWash . traverse . _Exempt)
              && null (x ^.. closing . closingWash . traverse . _Exempt),
            x
          )
      )
  where
    go :: [(Bool, Annotated Entry)] -> [(Bool, Annotated Entry)]
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

    gains :: Closing -> Amount 6
    gains c =
      let p = c ^. closingLot . price
          b = c ^. closingPos . posBasis
       in case c ^. closingPos . posDisp of
            Long -> p - b
            Short -> b - p

    eligibleClose ::
      [(Bool, Annotated Entry)] -> Zippered (Bool, Annotated Entry)
    eligibleClose = zippered $ \(w, x) ->
      w && gains (x ^?! item . _Event . _Close) < 0

    eligibleOpen anchor (w, y) =
      w && abs (anchor `distance` (y ^. time)) <= 30

    handleOpen ::
      (Bool, Annotated Entry) ->
      Bool ->
      Zippered (Bool, Annotated Entry) ->
      Maybe
        ( Zippered (Bool, Annotated Entry),
          [(Bool, Annotated Entry)]
        )
    handleOpen x _inPast part = do
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
                & _2 . closing . closingPos .~ o'
                & _2 . closing . closingLot .~ l
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
    dataset :: [Entry]
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
