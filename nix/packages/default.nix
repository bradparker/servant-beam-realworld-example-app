{ compiler ? "default"
}:
let
  haskellPackages = compiler: nixpkgs:
    if compiler == "default"
      then nixpkgs.haskellPackages
      else nixpkgs.haskell.packages.${compiler};
in
  import ./nixpkgs {
    config = {
      packageOverrides = nixpkgs: {
        haskellPackages = (haskellPackages compiler nixpkgs).override {
          overrides = self: super:
            (import ./beam nixpkgs self super) //
            (import ./validation nixpkgs self super);
        };
      };
    };
  }
