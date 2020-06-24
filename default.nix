{ ghcCompiler ? "ghc883"
, coqPackages ? "coqPackages_8_11"
, rev         ? "dcb64ea42e64aaecd8e6fef65cc86245c9666818"
, sha256      ? "0i77sgs0gic6pwbkvk9lbpfshgizdrqyh18law2ji1409azc09w0"
, pkgs        ? import (builtins.fetchTarball {
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
}:

let haskellPackages = pkgs.haskell.packages.${ghcCompiler};
in with pkgs.${coqPackages};
{
  ledger-annotate =
    pkgs.callPackage ./ledger-annotate { inherit ghcCompiler; };
  ledger-parse =
    pkgs.callPackage ./ledger-parse { inherit ghcCompiler; };
  options-model =
    pkgs.callPackage ./options-model { inherit coqPackages; };
  thinkorswim-api =
    pkgs.callPackage ./thinkorswim-api { inherit ghcCompiler; };
  thinkorswim-csv =
    pkgs.callPackage ./thinkorswim-csv { inherit ghcCompiler; };
  thinkorswim-el =
    pkgs.callPackage ./thinkorswim-el { inherit ghcCompiler; };
  trade-journal =
    pkgs.callPackage ./trade-journal { inherit ghcCompiler; };
}
