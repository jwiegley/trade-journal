# trade-journal

I've been managing my own brokerage trades for a while now, and the tax
reporting side has always been the worst part. Spreadsheets break down once you
hit a few hundred trades, and the wash sale rule makes everything harder than it
ought to be. This project is my attempt to get it right -- processing raw broker
exports all the way through to properly adjusted tax numbers, with exact
arithmetic and no rounding surprises.

## What it does

The data flows through five stages:

1. **Import** raw CSV from your broker
2. **Convert** those CSV records into a canonical stream of `Entry` values
3. **Calculate** the open/close disposition of buys and sells (FIFO by default)
4. **Compute** profit and loss from those events
5. **Apply** tax adjustments -- currently the US wash sale rule per IRS
   Publication 550

Output is formatted for [ledger-cli](https://ledger-cli.org/), giving you
proper double-entry bookkeeping from your trade history.

## Building

The project uses Nix for reproducible builds:

```bash
nix build                    # Build the trade executable
nix develop                  # Enter a development shell with all tools
nix flake check              # Run all checks (build, format, lint)
```

Inside the dev shell:

```bash
cabal build                  # Build everything
cabal haddock                # Generate documentation
```

## Usage

```bash
# Process a journal file
trade -f journal.dat -b broker-name

# Process a Coinmetro CSV export
trade coinmetro coinmetro-export.csv
```

## Supported brokers

- **Coinmetro** -- fully implemented
- **ThinkOrSwim** -- parser and types in place, conversion in progress
- **Coinbase** -- parser and types in place, conversion in progress

Adding a new broker requires three files under `src/Trade/Provider/`: a types
module (CSV schema via cassava), a parser, and a process module that converts to
`Entry`. See the Coinmetro implementation for the pattern.

## How it works

The interesting bit is the wash sale detection. IRS rules say that if you sell
at a loss and buy substantially identical stock within 30 days before *or*
after, the loss gets disallowed. Implementing this correctly means you need to
look both forward and backward in time from each closing trade.

I'm using a comonadic zipper -- generated from Agda proofs via
[agda2hs](https://github.com/agda/agda2hs) -- that gives O(1) access to
surrounding context. The `survey` function walks the zipper across the position
list, applying the wash sale logic at each step. The zipper module at
`src/Trade/Journal/Zipper.hs` is generated; don't edit it directly.

All financial calculations use `Amount n` backed by `Rational`, not floating
point. Rounding errors in tax calculations are the kind of bug nobody wants.

## Development

```bash
nix develop                  # Full dev shell
lefthook install             # Set up pre-commit hooks
```

Pre-commit hooks run formatting checks (fourmolu), linting (hlint), shell
script analysis (shellcheck), and a full `nix build` to catch regressions.
Everything runs in parallel.

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md).
