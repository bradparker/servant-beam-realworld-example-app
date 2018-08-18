packages:
config:
let
  inherit (import ../nixpkgs config)
    fetchFromGitHub
    lib;

  beam-source = fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };

  beam-core = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-core";
    src = "${beam-source}/beam-core";
  }) config;

  beam-migrate = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-migrate";
    src = "${beam-source}/beam-migrate";
  }) (config // {
    inherit beam-core;
  });

  beam-postgres = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-postgres";
    src = "${beam-source}/beam-postgres";
  }) (config // {
    inherit beam-core;
    inherit beam-migrate;
  });
in
  {
    inherit beam-core;
    inherit beam-migrate;
    inherit beam-postgres;
  }
