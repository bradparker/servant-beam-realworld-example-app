{ compiler
, nixpkgs
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
in
  [
    nixpkgs.direnv
    nixpkgs.postgresql
    nixpkgs.sqitchPg
    packages.cabal-install
    packages.ghcid
    packages.hindent
    packages.hlint
    packages.hpack
  ]
