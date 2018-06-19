{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};

  cabal = packages.cabal-install;
  direnv = nixpkgs.direnv;
  ghcid = packages.ghcid;
  hindent = packages.hindent;
  hlint = packages.hlint;
  postgresql = nixpkgs.postgresql;

  env = (import ./. {
    inherit nixpkgs;
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [
      cabal
      direnv
      ghcid
      hindent
      hlint
      postgresql
    ];
    shellHook = drv.shellHook + "
      mkdir -p $PWD/database/pgdata
      export PGDATA=$PWD/database/pgdata

      mkdir -p $PWD/logs
    ";
  })
