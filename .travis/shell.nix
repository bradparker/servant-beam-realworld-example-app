let
  compiler = "default";
  nixpkgs = import ../nix/packages/nixpkgs {};

  tools = import ./tools.nix {
    inherit nixpkgs;
    inherit compiler;
  };

  env = (import ../. {
    check = true;
    inherit nixpkgs;
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs =
      drv.nativeBuildInputs ++
      tools;
  })
