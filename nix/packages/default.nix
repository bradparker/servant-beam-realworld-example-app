import ./nixpkgs {
  config = {
    packageOverrides = nixpkgs: {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: {
          inherit (import ./beam nixpkgs self super)
            beam-core
            beam-postgres
            beam-migrate;
          inherit (import ./base-noprelude nixpkgs self super)
            base-noprelude;
        };
      };
    };
  };
}
