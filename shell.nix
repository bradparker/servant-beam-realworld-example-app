{ compiler ? "default"
}:
let
  nixpkgs = import ./nix/packages {
    inherit compiler;
  };

  tools = import ./nix/tools.nix nixpkgs;

  env = (import ./. {
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
