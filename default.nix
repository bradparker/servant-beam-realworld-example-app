{ nixpkgs ? import ./nix/packages/nixpkgs {}
, compiler ? "default"
, check ? false
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};
  checker = if check
    then nixpkgs.haskell.lib.doCheck
    else nixpkgs.haskell.lib.dontCheck;
in
  checker (packages.callPackage ./package.nix {
    inherit (import ./nix/packages/beam packages {})
      beam-core
      beam-postgres;

    inherit (import ./nix/packages/validation packages {})
      validation;
  })
