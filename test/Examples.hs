{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples (testExamples) where

import Control.Monad.Writer
import Data.List (intersperse)
import Data.String.Here.Interpolated
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Journal.Parse
import Journal.Pipes
import Pipes
import Test.Tasty
import Test.Tasty.HUnit

testExamples :: TestTree
testExamples =
  testGroup
    "examples"
    [ baseline,
      realWorld
    ]

baseline :: TestTree
baseline =
  testGroup
    "baseline"
    [ testCase "journal-buy-sell-profit" journalBuySellProfit,
      testCase "journal-buy-sell-loss-buy" journalBuySellLossBuy
    ]

journalBuySellProfit :: Assertion
journalBuySellProfit = ii @--> oo
  where
    ii =
      [i|
2020-07-02 buy 100 AAPL 260.00
2020-07-03 sell 100 AAPL 300.00 fees 0.20
        |]
    oo =
      [i|
2020-07-02 00:00:00 buy 100 AAPL 260.0000
2020-07-02 00:00:00 open long 100 AAPL 260.0000 ids [1]
2020-07-03 00:00:00 sell 100 AAPL 300.0000 fees 0.20
2020-07-03 00:00:00 close long 100 AAPL 300.0000 gain 3999.80 fees 0.20 ids [1]
        |]

journalBuySellLossBuy :: Assertion
journalBuySellLossBuy = ii @--> oo
  where
    ii =
      [i|
2020-07-02 buy 100 AAPL 260.00
2020-07-03 sell 100 AAPL 240.00 fees 0.20 wash to A
2020-07-04 buy 100 AAPL 260.00 apply A 100
        |]
    oo =
      -- The final wash present date is incorrect
      [i|
2020-07-02 00:00:00 buy 100 AAPL 260.0000
2020-07-02 00:00:00 open long 100 AAPL 260.0000 ids [1]
2020-07-03 00:00:00 sell 100 AAPL 240.0000 fees 0.20 wash to A
2020-07-03 00:00:00 close long 100 AAPL 240.0000 loss 2000.20 fees 0.20 wash to A ids [1]
2020-07-03 00:00:00 wash future 2020-07-03 00:00:00 100 AAPL 20.0020 fees 0.20 wash to A
2020-07-04 00:00:00 buy 100 AAPL 260.0000 apply A 100
2020-07-04 00:00:00 open long 100 AAPL 260.0000 apply A 100 ids [2]
  washed 2000.200000
2020-07-04 00:00:00 wash present 2020-07-04 00:00:00 100 AAPL 20.0020 apply A 100
        |]

realWorld :: TestTree
realWorld =
  testGroup
    "real-world"
    []

-- testCase "zoom-history" zoomHistory

{-
zoomHistory :: Assertion
zoomHistory = ii @--> oo
  where
    ii =
      [i|
| Trns |   Qty | Open  |    Basis |    Price | Gain ($) |    |                     |
|------+-------+-------+----------+----------+----------+----+---------------------|
| Eqty |   140 | 06/24 |  99.7792 |          |          |    |                     |
| Eqty |    10 | 06/24 |   89.785 |          |          |    |                     |
| Eqty |    30 | 06/24 |   106.68 |          |          |    |                     |
| Eqty |   170 | 06/25 |  85.8415 |          |          |    |                     |

2019-06-24 buy 140 ZM 99.7792 exempt
2019-06-24 buy 10 ZM 89.785 exempt
2019-06-24 buy 30 ZM 106.68 exempt
2019-06-25 buy 170 ZM 85.8415 exempt

2019-07-01 buy 50 ZM 85.80
| Buy  |    50 |       |          |    85.80 |          |    |                     |

2019-07-01 sell 50 ZM 86.675 fees 0.10
| Sell |    50 | 06/24 |  99.7792 |   86.673 |  -655.31 |    |                     |
| Wash |  [50] | 07/01 |          |          |   655.31 | == | 85.80 -> 98.9062    |

2019-07-02 buy 50 ZM 85.50
| Buy  |    50 |       |          |    85.50 |          |    |                     |

2019-07-03 sell 100 ZM 86.7765 fees 0.19 wash to A
| Sell |    90 | 06/24 |  99.7792 |  86.7746 | -1170.42 |    |                     |
| Wash |  [50] | 07/02 |          |          |   650.23 | == | 85.50 -> 98.5046    |
| Wash |  [40] | 07/29 |          |          |   520.19 | A. |                     |
| Sell |    10 | 06/24 |   89.785 |   86.775 |   -30.10 |    |                     |
| Wash |  [10] | 07/29 |          |          |    30.10 | A. |                     |

2019-07-03 sell 100 ZM 88.14 fees 0.20 wash to A
| Sell |    30 | 06/24 |   106.68 |   88.138 |  -556.26 |    |                     |
| Wash |  [30] | 07/29 |          |          |   556.26 | A> | 95.7852 -> 98.5516  |
| Sell |    70 | 06/25 |  85.8415 |  88.1381 |   160.76 |    |                     |

2019-07-12 sell 200 ZM 94.085 fees 0.41 wash to B
| Sell |   100 | 06/25 |  85.8415 |   94.083 |   824.15 |    |                     |
| Sell |    50 | 07/01 |  98.9062 |  94.0828 |  -241.17 |    |                     |
| Sell |    50 | 07/02 |  98.5046 |   94.083 |  -221.08 |    |                     |
| Wash |  [50] | 07/29 |          |          |   241.17 | B. |                     |
| Wash |  [50] | 07/29 |          |          |   221.08 | B> | 95.7852 -> 100.4077 |

2019-07-29 buy 500 ZM 95.7852 apply A 400 apply B 100
| Buy  |   400 |       |          |  98.5516 |          | <A |                     |
| Buy  |   100 |       |          | 100.4077 |          | <B |                     |

2019-07-29 sell 400 ZM 95.657 fees 0.84 wash 200 @ 0.1303 to C
| Sell |   400 | 07/29 |  98.5516 |  95.6549 | -1158.67 |    |                     |
| Wash | [200] | 07/30 |          |          |    26.06 | C> | 96.00 -> 96.1303    |

2019-07-29 sell 100 ZM 96.1841 fees 0.21
| Sell |   100 | 07/29 | 100.4077 |   96.182 |  -422.57 |    |                     |

2019-07-30 buy 200 ZM 96.00 apply C 200
| Buy  |   200 |       |          |  96.1303 |          | <C |                     |
2019-09-06 buy 100 ZM 85.97 commission 5.00
| Buy  |   100 |       |          |    86.02 |          |    |                     |

2020-02-18 sell 300 ZM 95.00 fees 0.67
| Sell |   200 | 07/30 | 96.1303  |   95.00  |   897.78 |    |                     |
| Sell |   100 | 09/06 | 85.9245  |   95.00  |  -226.51 |    |                     |
        |]
    oo =
      [i|
2019-06-24 00:00:00 buy 140 ZM 99.7792 open exempt
2019-06-24 00:00:00 buy 10 ZM 89.7850 open exempt
2019-06-24 00:00:00 buy 30 ZM 106.6800 open exempt
2019-06-25 00:00:00 buy 170 ZM 85.8415 open exempt
2019-07-01 00:00:00 buy 50 ZM 85.8000 open
2019-07-01 00:00:00 sell 50 ZM 86.6750 close fees 0.10
  loss 655.310000
2019-07-01 00:00:00 wash 50 ZM 13.1062
  washed 4290.000000
2019-07-02 00:00:00 buy 50 ZM 85.5000 open
2019-07-03 00:00:00 sell 90 ZM 86.7765 close fees 0.171 wash to A
  loss 1170.414000
2019-07-03 00:00:00 wash 50 ZM 13.0046
  washed 4275.000000
2019-07-03 00:00:00 wash 40 ZM 13.0046 fees 0.076 wash to A
2019-07-03 00:00:00 sell 10 ZM 86.7765 close fees 0.019 wash to A
  loss 30.104000
2019-07-03 00:00:00 wash 10 ZM 3.0104 fees 0.019 wash to A
2019-07-03 00:00:00 sell 30 ZM 88.1400 close fees 0.06 wash to A
  loss 556.260000
2019-07-03 00:00:00 wash 30 ZM 18.5420 fees 0.06 wash to A
2019-07-03 00:00:00 sell 70 ZM 88.1400 close fees 0.14 wash to A
  gain 160.755000
2019-07-12 00:00:00 sell 100 ZM 94.0850 close fees 0.205 wash to B
  gain 824.145000
2019-07-12 00:00:00 sell 50 ZM 94.0850 close fees 0.1025 wash to B
  loss 241.162500
2019-07-12 00:00:00 wash 50 ZM 4.82325 fees 0.1025 wash to B
2019-07-12 00:00:00 sell 50 ZM 94.0850 close fees 0.1025 wash to B
  loss 221.082500
2019-07-12 00:00:00 wash 50 ZM 4.42165 fees 0.1025 wash to B
2019-07-29 00:00:00 buy 400 ZM 95.7852 open apply A 400 apply B 100
  washed 1106.548000
2019-07-29 00:00:00 buy 100 ZM 95.7852 open apply A 400 apply B 100
  washed 462.245000
2019-07-29 00:00:00 sell 400 ZM 95.6570 close fees 0.84 wash 200 @ 0.1303 to C
  loss 1158.668000
2019-07-29 00:00:00 wash 400 ZM 2.89667 fees 0.84 wash 200 @ 0.1303 to C
2019-07-29 00:00:00 sell 100 ZM 96.1841 close fees 0.21
  loss 422.565000
2019-07-30 00:00:00 buy 200 ZM 96.0000 open apply C 200
  washed 26.060000
2019-09-06 00:00:00 buy 100 ZM 85.9700 open commission 5.00
2020-02-18 00:00:00 sell 200 ZM 95.0000 close fees 0.446667
  loss 226.506667
2020-02-18 00:00:00 sell 100 ZM 95.0000 close fees 0.223333
  gain 897.776667
        |]
-}

combine :: MonadWriter [T.Text] m => Consumer T.Text m r
combine = forever $ do
  msg <- await
  lift $ tell [msg]

(@-->) :: Text -> Text -> Assertion
x @--> y = do
  (_, msgs) <-
    runWriterT $
      parseProcessPrint (parseActionsAndEventsFromText "" x) combine
  let y' = TL.intercalate "\n" (map TL.fromStrict msgs)
  trimLines y' @?= trimLines y
  where
    trimLines =
      TL.concat
        . intersperse "\n"
        . map TL.strip
        . TL.splitOn "\n"
        . TL.strip
