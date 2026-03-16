{
  description = "Trade journal software";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          trade-journal =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                  hlint = {};
                };
                withHoogle = true;
                buildInputs = with final; [
                  pkg-config
                  # Formatting
                  haskellPackages.fourmolu
                  # Shell script tools
                  shellcheck
                  shfmt
                  # Git hooks
                  lefthook
                ];
              };
              modules = [{
                enableLibraryProfiling = true;
                enableProfiling = true;
              }];
            };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.trade-journal.flake {};
    in {
      packages.default = flake.packages."trade-journal:exe:trade";

      devShells.default = flake.devShells.default;

      checks = {
        # Full build (includes -Wall -Werror)
        build = self.packages.${system}.default;

        # Haskell formatting (Zipper.hs excluded -- generated from Agda)
        formatting = pkgs.runCommand "check-formatting" {
          nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
        } ''
          cd ${self}
          find src bin -name '*.hs' -not -name 'Zipper.hs' \
            -exec fourmolu --mode check {} +
          touch $out
        '';

        # Haskell linting (Zipper.hs excluded -- generated from Agda)
        hlint = pkgs.runCommand "check-hlint" {
          nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
        } ''
          cat > $TMPDIR/.hlint.yaml <<'HLINT'
          - ignore: {name: "Reduce duplication"}
          - ignore: {name: "Use newtype instead of data"}
          HLINT
          cd ${self}
          find src bin -name '*.hs' -not -name 'Zipper.hs' \
            -exec hlint --hint=$TMPDIR/.hlint.yaml {} +
          touch $out
        '';

        # Shell script linting
        shellcheck = pkgs.runCommand "check-shellcheck" {
          nativeBuildInputs = [ pkgs.shellcheck ];
        } ''
          cd ${self}
          shellcheck build
          touch $out
        '';

        # Shell script formatting
        shell-format = pkgs.runCommand "check-shell-format" {
          nativeBuildInputs = [ pkgs.shfmt ];
        } ''
          cd ${self}
          shfmt -d build
          touch $out
        '';

        # TODO: Uncomment when tests are re-enabled in package.yaml
        # tests = flake.packages."trade-journal:test:journal-tests";

        # TODO: Add code coverage check once tests are working
        # coverage = pkgs.runCommand "check-coverage" { ... } ''
        #   cabal test --enable-coverage
        #   hpc-codecov ...
        # '';
      };
    });
}
