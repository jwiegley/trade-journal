# CLAUDE.md - AI Assistant Guide for trade-journal

This project manages and tracks trades placed with brokers (buy/sells of instruments) and related account events (interest, dividends, etc). It processes real trades and calculates tax obligations including wash sale adjustments per IRS rules.

## 🚨 Critical Safety Rules

!!! danger "MANDATORY BEFORE ANY CHANGES"
    1. **RE-ENABLE TESTS FIRST**: Tests are currently disabled in `package.yaml`. Uncomment and run all tests before modifying business logic.
    2. **NEVER USE FLOAT/DOUBLE**: All financial amounts MUST use `Amount n` with Rational backend to prevent rounding errors.
    3. **DO NOT MODIFY ZIPPER.HS DIRECTLY**: This file is generated from Agda proofs. Regenerate from `src/Trade/Journal/Zipper.agda` instead.
    4. **WASH SALE CHANGES REQUIRE TAX EXPERTISE**: The wash sale algorithm implements IRS Publication 550 rules. Consult tax documentation before changes.
    5. **MAINTAIN POSITION INVARIANTS**: You cannot close more shares than are open. Use `validPositionChange` to verify.

## Common Commands

```bash
# Build
nix build

# Development shell
nix develop

# Run tests (MUST uncomment test sections in package.yaml first)
cabal test --test-show-details=direct

# Run specific test suite
cabal test journal-tests

# Run with profiling
cabal run trade --enable-profiling -- +RTS -p -RTS process journal.dat

# Process journal file
./result/bin/trade process journal.dat

# Process broker CSV
./result/bin/trade coinmetro coinmetro-export.csv

# Regenerate Zipper.hs from Agda (DO NOT edit Zipper.hs directly)
make src/Trade/Journal/Zipper.hs

# Build documentation
cabal haddock --hyperlink-source
```

## Data Processing Pipeline

The code follows a 5-stage pipeline (from README.md):

1. **Raw CSV import** from broker
2. **Convert CSV** → stream of `Entry` values
3. **Calculate Open/Close** disposition of buys and sells
4. **Calculate profit/loss** from Open/Close events
5. **Apply tax adjustments** (wash sale rule for US taxes)

## High-Level Architecture

### Cross-Module Data Flow

The architecture requires understanding how data flows across multiple modules:

```
Trade.Provider.*.Parser (CSV parsing)
    ↓ [Transaction records]
Trade.Provider.*.Process (broker-specific conversion)
    ↓ [Journal.Entry stream]
Trade.Journal.Process.applyLot (FIFO/LIFO position tracking)
    ↓ [Position list with PositionChange events]
Trade.Journal.Process.washSales (comonadic zipper traversal)
    ↓ [Position list with wash adjustments]
Ledger.Entry (convert to double-entry bookkeeping)
    ↓ [Transaction with Postings]
Ledger.Render (format for ledger-cli)
    ↓ [Text output]
```

### Key Architectural Decisions

**Comonadic Zipper for Wash Sales**: The wash sale detection requires looking 30 days before/after each trade. Rather than passing the entire list, we use a zipper (generated from Agda proofs) that provides O(1) access to surrounding context:

```haskell
-- src/Trade/Journal/Process.hs:washSales uses survey from Zipper.hs
washSales = survey $ \(MkZipper before current after) -> ...
```

**Type-Level Precision Tracking**: Financial amounts use phantom types to track decimal places at compile time, preventing precision mixing errors:

```haskell
-- vendor/simple-amount/src/Amount.hs
newtype Amount (n :: Nat) = Amount Rational
-- Used throughout as Amount 2 (dollars), Amount 6 (shares), etc.
```

**Provider Adapter Pattern**: Each broker has three files working together:
- `Types.hs`: Defines CSV schema with cassava `FromNamedRecord`
- `Parser.hs`: Reads CSV file into vector of records
- `Process.hs`: Converts broker-specific records to canonical `Journal.Entry`

This separation allows adding new brokers without modifying core logic.


## Key Type System Patterns

### Position State Machine
```haskell
data Position = Open OpenPosition | Closed ClosedPosition

data PositionChange
  = PositionOpen Lot              -- New position
  = PositionIncrease OpenPosition Amount  -- Add to existing
  = PositionPartialClose OpenPosition Lot -- Reduce position
  = PositionClose OpenPosition TimePrice  -- Complete close
```

### Error Handling Migration
Current code uses `error` (crashes on invalid input). New code must use `Either`:
```haskell
-- Old pattern (avoid)
readAmount s = error $ "Failed: " ++ s

-- New pattern (use this)
readAmount :: String -> Either ParseError (Amount n)
```

## Wash Sale Algorithm

The wash sale rule (IRS Publication 550) states: if you sell stock at a loss and buy "substantially identical" stock within 30 days before or after, the loss is disallowed for tax purposes. The implementation uses a comonadic zipper to look ±30 days:

```haskell
-- src/Trade/Journal/Process.hs
washSales :: [Position] -> [Position]
washSales = survey go
  where
    go (MkZipper before current after)
      | eligibleLosingClose current =
          case findWashIn30Days before after of
            Just replacement -> adjustBasis replacement current
            Nothing -> current
      | otherwise = current
```

Example:
- Day 1: Buy 100 AAPL @ $150
- Day 15: Sell 100 AAPL @ $140 (Loss: $1,000)
- Day 20: Buy 100 AAPL @ $145
- Result: $1,000 loss deferred, Day 20 basis = $155

## Adding a New Broker

Required files for each broker:

1. `src/Trade/Provider/NewBroker/Types.hs` - CSV schema with `FromNamedRecord`
2. `src/Trade/Provider/NewBroker/Parser.hs` - Read CSV using cassava
3. `src/Trade/Provider/NewBroker/Process.hs` - Convert to `Journal.Entry`

Then add to `bin/Main.hs`:
```haskell
"newbroker" -> do
  transactions <- NewBroker.Parser.readCsv csvPath
  let entries = NewBroker.Process.toEntries transactions
  processJournal entries
```

See existing providers (Coinmetro, ThinkOrSwim, Coinbase) for examples.

## Testing

**WARNING**: Tests are currently disabled in `package.yaml`. Must uncomment before running.

Key test files:
- `tests/Journal/TestWashSaleRule.hs` - Wash sale property tests
- `tests/Journal/TestPositions.hs` - Position tracking invariants

Critical invariants to test:
```haskell
-- Cannot close more shares than open
prop_no_overdose :: [PositionChange] -> Bool

-- Positions must balance (open + closed = 0)
prop_position_conservation :: [Trade] -> Bool

-- Wash sales must be within 30 days
prop_wash_sale_window :: [Position] -> Bool
```

## Common Issues

### Tests disabled
Tests are commented out in `package.yaml`. Uncomment test sections before running `cabal test`.

### Wash sale not detected
Check: 30-day window calculation, loss verification (`salePrice < purchasePrice`), not already washed.

### Position invariant violated
Cannot close more shares than open. Use `validPositionChange` to verify.

### CSV parsing failures
Current code uses `error` on invalid input. Refactor to use `Either` for graceful handling.

## References

- [IRS Publication 550](https://www.irs.gov/publications/p550) - Wash sale rules
- [agda2hs](https://github.com/agda/agda2hs) - Generates Zipper.hs from Agda