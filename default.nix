{ compiler ? "ghc8107"

, rev    ? "61d24cba72831201efcab419f19b947cf63a2d61"
, sha256 ? "10yi7pp764vz0ikplqysdbyw104gwh967yxis5zizcz0jksc27jn"

, pkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage rec {
  name = "haskell-${compiler}-trade-journal";
  root = ./.;

  source-overrides = {};
  overrides = self: super: with pkgs.haskell.lib; {
    simple-amount = import ./vendor/simple-amount {
      inherit pkgs; returnShellEnv = false;
    };
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.hoogle
      haskellPackages.hasktags
      haskellPackages.ghcid
      haskellPackages.ormolu
      haskellPackages.haskell-language-server
    ];

    libraryHaskellDepends = (attrs.libraryHaskellDepends or []) ++ [];

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
