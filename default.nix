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

let
  haskellPackages = pkgs.haskell.packages.${compiler};

  agda2hs-src = pkgs.fetchFromGitHub {
    owner = "agda";
    repo = "agda2hs";
    rev = "160478a51bc78b0fdab07b968464420439f9fed6";
    sha256 = "13k2lcljgq0f5zbbyyafx1pizw4ln60xi0x0n0p73pczz6wdpz79";
    # date = 2021-09-08T18:00:00+02:00;
  };

  agda2hs = haskellPackages.developPackage rec {
    name = "agda2hs";
    root = agda2hs-src;

    source-overrides = {};
    overrides = self: super: with pkgs.haskell.lib; {};

    modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
      buildTools = (attrs.buildTools or []) ++ [
        haskellPackages.cabal-install
      ];

      passthru = {
        nixpkgs = pkgs;
        inherit haskellPackages;
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

  source-overrides = {};
  overrides = self: super: with pkgs.haskell.lib; {
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
      haskell-language-server
      agda2hs
      (pkgs.agdaPackages.agda.withPackages (p: [
         p.standard-library p.agda-categories agda2hs-lib
       ]))
    ]);

    libraryHaskellDepends = (attrs.libraryHaskellDepends or []) ++ [];

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
