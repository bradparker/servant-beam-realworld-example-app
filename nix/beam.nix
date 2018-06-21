with import <nixpkgs> {};
{ packages
}:
let
  beam-source = fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    inherit (builtins.fromJSON (builtins.readFile ./beam.json)) rev sha256;
  };
  beam-core = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-core";
    src = "${beam-source}/beam-core";
  }) {};
  beam-migrate = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-migrate";
    src = "${beam-source}/beam-migrate";
  }) { inherit beam-core; };
  beam-postgres = packages.callPackage (packages.haskellSrc2nix {
    name = "beam-postgres";
    src = "${beam-source}/beam-postgres";
  }) {
    inherit beam-core;
    inherit beam-migrate;
  };
in
  {
    inherit beam-core;
    inherit beam-migrate;
    inherit beam-postgres;
  }
