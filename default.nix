{ compiler ? "ghc923"

, rev    ? "bd4dffcdb7c577d74745bd1eff6230172bd176d5"
, sha256 ? "18zacrykj2k5x42d0grr7g1y7xhy5ppq7j0gm3lrghwflyrdkslj"

, pkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  haskellPackages_8_10 = pkgs.haskell.packages.ghc8107;

  agda2hs-src = pkgs.fetchFromGitHub {
    owner = "agda";
    repo = "agda2hs";
    rev = "160478a51bc78b0fdab07b968464420439f9fed6";
    sha256 = "13k2lcljgq0f5zbbyyafx1pizw4ln60xi0x0n0p73pczz6wdpz79";
    # date = 2021-09-08T18:00:00+02:00;
  };

  agda2hs = haskellPackages_8_10.developPackage rec {
    name = "agda2hs";
    root = agda2hs-src;

    source-overrides = {};
    overrides = self: super: with pkgs.haskell.lib; {};

    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [
        haskellPackages_8_10.cabal-install
      ];

      passthru = {
        nixpkgs = pkgs;
        haskellPackages = haskellPackages_8_10;
      };
    });

    returnShellEnv = false;
  };

  agda2hs-lib = pkgs.stdenv.mkDerivation rec {
    name = "agda2hs-${version}";
    version = "1.0";

    src = agda2hs-src;

    buildInputs = [
      (pkgs.agdaPackages.agda.withPackages (p: [p.standard-library]))
    ];

    libraryFile = "${libraryName}.agda-lib";
    libraryName = "agda2hs";

    phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];

    buildPhase = ''
      agda -i lib lib/Haskell/Prelude.agda
    '';

    installPhase = ''
      mkdir -p $out
      cp -pR * $out
    '';
  };

in haskellPackages.developPackage rec {
  name = "haskell-${compiler}-trade-journal";
  root = ./.;

  source-overrides = {
    fastsum = "0.2.0.0";
  };
  overrides = self: super: with pkgs.haskell.lib; {
    hashtables = doJailbreak super.hashtables;
    fastsum = doJailbreak super.fastsum;
    simple-amount = import ./vendor/simple-amount {
      inherit pkgs; returnShellEnv = false;
    };
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ (with haskellPackages; [
      cabal-install
      hpack
      hoogle
      hasktags
      ghcid
      ormolu
      #agda2hs
      #(pkgs.agdaPackages.agda.withPackages (p: [
      #   p.standard-library p.agda-categories agda2hs-lib
      # ]))
    ]);

    libraryHaskellDepends = (attrs.libraryHaskellDepends or []) ++ [
      haskellPackages.simple-amount
    ];

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
