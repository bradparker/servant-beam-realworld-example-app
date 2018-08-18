{ compiler ? "default"
, nixpkgs ? import ./nix/packages/nixpkgs {}
}:
let
  tools = import ./nix/tools.nix {
    inherit nixpkgs;
    inherit compiler;
  };

  env = (import ./. {
    check = true;
    inherit nixpkgs;
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs =
      drv.nativeBuildInputs ++
      tools;
    shellHook = drv.shellHook + "
      mkdir -p $PWD/database/pgdata
      export PGDATA=$PWD/database/pgdata

      mkdir -p $PWD/logs
    ";
  })
