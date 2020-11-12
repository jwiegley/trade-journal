{ ghcCompiler ? "ghc884"

, doBenchmark ? false
, doProfiling ? true
, doStrict    ? true

, rev    ? "970b2b853d41ec80a3c2aba3e585f52818fbbfa3"
, sha256 ? "0cwm2gvnb7dfw9pjrwzlxb2klix58chc36nnymahjqaa1qmnpbpq"

, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
    overlays = [
      (self: super:
       let
         nixpkgs = { rev, sha256 }:
           import (super.fetchFromGitHub {
             owner = "NixOS";
             repo  = "nixpkgs";
             inherit rev sha256;
           }) { config.allowUnfree = true; };

         known-good-20191113_070954 = nixpkgs {
           rev    = "620124b130c9e678b9fe9dd4a98750968b1f749a";
           sha256 = "0xgy2rn2pxii3axa0d9y4s25lsq7d9ykq30gvg2nzgmdkmy375rr";
         };
       in
       {
         inherit (known-good-20191113_070954) shared-mime-info;

         haskell = super.haskell // {
           packages = super.haskell.packages // {
             ${ghcCompiler} = super.haskell.packages.${ghcCompiler} //
               (let hask = super.haskell.packages.${ghcCompiler};
                in with super.haskell.lib; rec {
                  ghc = super.haskell.packages.${ghcCompiler}.ghc //
                    { withPackages =
                        super.haskell.packages.${ghcCompiler}.ghc.withHoogle; };
                  ghcWithPackages = ghc.withPackages;
                });
           };
         };
       })
    ];
  }

, mkDerivation ? null
, returnShellEnv ? pkgs.lib.inNixShell
}:

let haskellPackages = pkgs.haskell.packages.${ghcCompiler};

in haskellPackages.developPackage rec {
  name = "haskell-${ghcCompiler}-trade-journal";
  root = ./.;

  source-overrides = {
  };
  overrides = self: super: with pkgs.haskell.lib; {
    http-media          = doJailbreak super.http-media;
    servant-client-core = super.servant-client-core_0_17;
    servant-client      = super.servant-client_0_17;
    servant-server      = super.servant-server_0_17;
    servant             = super.servant_0_17;
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.hoogle
      haskellPackages.hasktags
      haskellPackages.ghcid
      haskellPackages.ormolu
      pkgs.mpfr.out
      pkgs.mpfr.dev
    ];

    enableLibraryProfiling = doProfiling;
    enableExecutableProfiling = doProfiling;

    testHaskellDepends = (attrs.testHaskellDepends or []) ++ [
      # haskellPackages.criterion
    ];

    inherit doBenchmark;

    configureFlags =
      pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };

    shellHook = ''
      CABAL_REPL="cabal repl --extra-lib-dirs=${pkgs.mpfr.out}/lib \
                             --extra-lib-dirs=${pkgs.gmp.out}/lib"
      export CABAL_REPL
    '';
  });

  inherit returnShellEnv;
}
