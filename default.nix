{ compiler ? "ghc8107"

, rev    ? "a3a23d9599b0a82e333ad91db2cdc479313ce154"
, sha256 ? "05xmgrrnw6j39lh3d48kg064z510i0w5vvrm1s5cdwhdc2fkspjq"
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

  source-overrides = {
    # fastsum = "0.2.0";
  };
  overrides = self: super: with pkgs.haskell.lib; {
    # fastsum = import ../fastsum {
    #   inherit pkgs; returnShellEnv = false;
    # };
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

    libraryHaskellDepends = (attrs.libraryHaskellDepends or []) ++ [
      # (import ../fastsum {
      #   inherit pkgs; returnShellEnv = false;
      # })
    ];

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
