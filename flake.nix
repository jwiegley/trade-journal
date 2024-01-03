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
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.trade-journal.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          trade-journal =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc963";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in {
      packages.default = flake.packages."trade-journal:exe:trade-journal";
      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
