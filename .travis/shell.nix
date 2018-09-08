let
  compiler = "default";

  nixpkgs = import ../nix/packages {
    inherit compiler;
  };

  tools = import ./tools.nix nixpkgs;

  env = (import ../. {
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs =
      drv.nativeBuildInputs ++
      tools;
  })
