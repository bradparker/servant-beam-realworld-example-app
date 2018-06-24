{ compiler
, nixpkgs
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
in
  [
    nixpkgs.sqitchPg
    packages.cabal-install
  ]
