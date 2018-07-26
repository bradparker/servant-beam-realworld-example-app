{ nixpkgs ? import ./nix/nixpkgs.nix {}
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
    inherit (import ./nix/beam.nix packages {})
      beam-core
      beam-postgres;

    inherit (import ./nix/validation.nix packages {})
      validation;
  })
