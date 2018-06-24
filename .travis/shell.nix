let
  compiler = "default";
  nixpkgs = import ../nix/nixpkgs.nix {};

  tools = import ./tools.nix {
    inherit nixpkgs;
    inherit compiler;
  };

  env = (import ../. {
    inherit nixpkgs;
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs =
      drv.nativeBuildInputs ++
      tools;
  })
