let
  nixpkgs = import ./nix/packages;
in
  nixpkgs.haskellPackages.callPackage ./package.nix {}
