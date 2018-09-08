{ lib
, fetchFromGitHub
, ...
}:
self:
super:
let
  beam-source = fetchFromGitHub {
    owner = "tathougies";
    repo = "beam";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    beam-core = self.callPackage (self.haskellSrc2nix {
      name = "beam-core";
      src = "${beam-source}/beam-core";
    }) {};

    beam-migrate = self.callPackage (self.haskellSrc2nix {
      name = "beam-migrate";
      src = "${beam-source}/beam-migrate";
    }) {};

    beam-postgres = self.callPackage (self.haskellSrc2nix {
      name = "beam-postgres";
      src = "${beam-source}/beam-postgres";
    }) {};
  }
