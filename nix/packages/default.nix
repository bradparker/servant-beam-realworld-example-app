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
            {
              inherit (import ./beam nixpkgs self super)
                beam-core
                beam-migrate
                beam-postgres;
              inherit (import ./validation nixpkgs self super)
                validation;
            };
        };
      };
    };
  }
