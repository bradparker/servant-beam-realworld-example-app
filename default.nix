{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
in
  packages.callPackage ./package.nix {}
