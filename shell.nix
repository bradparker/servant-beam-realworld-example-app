let
  nixpkgs = import ./nix/packages;
  tools = import ./nix/tools.nix nixpkgs;
  package = import ./.;
  packageWithTools = nixpkgs.haskell.lib.addBuildDepends package tools;
in
  nixpkgs.lib.overrideDerivation packageWithTools.env (drv: {
    shellHook = drv.shellHook + "
      mkdir -p $PWD/database/pgdata
      export PGDATA=$PWD/database/pgdata

      mkdir -p $PWD/logs
    ";
  })
