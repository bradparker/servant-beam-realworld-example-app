{ nixpkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
}:
let
  packages = if compiler == "default"
    then nixpkgs.haskellPackages
    else nixpkgs.haskell.packages.${compiler};

  cabal = packages.cabal-install;
  ghcid = packages.ghcid;
  hindent = packages.hindent;
  hlint = packages.hlint;

  env = (import ./. {
    inherit nixpkgs;
    inherit compiler;
  }).env;
in
  nixpkgs.lib.overrideDerivation env (drv: {
    nativeBuildInputs = drv.nativeBuildInputs ++ [
      cabal
      ghcid
      hindent
      hlint
    ];
  })
