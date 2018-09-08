{ compiler ? "default"
}:
let
  nixpkgs = import ./nix/packages {
    inherit compiler;
  };
in
  nixpkgs.haskellPackages.callPackage ./package.nix {}
