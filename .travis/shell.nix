let
  nixpkgs = import ../nix/packages;
  tools = import ./tools.nix nixpkgs;
  package = import ../.;
in
  (nixpkgs.haskell.lib.addBuildDepends package tools).env
