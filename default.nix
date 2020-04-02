{ compiler ? "ghc882"

, doBenchmark ? false
, doProfiling ? false
, doStrict    ? true

, rev    ? "4a3f9aced7ff35ff8cd7e021a542121a66396586"
, sha256 ? "0jhx75jv4m9nrhandbvi41shpv3sbqgaqrh6az8fhfybw6b5w7ip"

, aeson-schema-f ?
    hask: (pkgs.haskell.lib.doJailbreak hask.aeson-schema).overrideAttrs(attrs: {
      src = pkgs.fetchFromGitHub {
        owner = "Fuuzetsu";
        repo = "aeson-schema";
        rev = "288c423f7c0037875636ea16cbd7fbb8e06dfc67";
        sha256 = "12damlxsbi08dbav9lqk5yvr7067vxpf98f4qfds6jinyhl7zsyj";
        # date = 2020-01-03T11:40:12+09:00;
      };
      patches = [ ./patch ];
    })

, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
    overlays = [
      (self: super: {
         haskell = super.haskell // {
           packages = super.haskell.packages // {
             ${compiler} = super.haskell.packages.${compiler} //
               (let hask = super.haskell.packages.${compiler};
                in with super.haskell.lib; rec {
                  aeson-schema = aeson-schema-f hask;
                  ghc = super.haskell.packages.${compiler}.ghc //
                    { withPackages =
                        super.haskell.packages.${compiler}.ghc.withHoogle; };
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

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage rec {
  name = "haskell-${compiler}-thinkorswim";
  root = ./.;

  source-overrides = {
  };
  overrides = self: super: with pkgs.haskell.lib; {
    aeson-schema        = aeson-schema-f haskellPackages;
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
      haskellPackages.aeson-schema
      haskellPackages.hoogle
    ];

    enableLibraryProfiling = doProfiling;
    enableExecutableProfiling = doProfiling;

    testHaskellDepends = (attrs.testHaskellDepends or []) ++ [
      haskellPackages.criterion
    ];

    inherit doBenchmark;

    configureFlags =
      pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
