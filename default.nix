{ ghcCompiler ? "ghc883"
, coqPackages ? "coqPackages_8_11"

, doBenchmark ? false
, doProfiling ? true
, doStrict    ? true

, rev    ? "4855aa62fa13052fb1a3daea68971503ab07a744"
, sha256 ? "17sk264qw397zzw12x11ry5vj9qidgbmjsyj544ysg857b4qq9sj"

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

in with pkgs.${coqPackages};

haskellPackages.developPackage rec {
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
      coq
      coq.ocaml
      coq.camlp5
      coq.findlib
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
      compatibleCoqVersions = v: builtins.elem v [ "8.11" ];
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
