{ lib
, fetchFromGitHub
, ...
}:
self:
super:
let
  validation-source = fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    validation = self.callPackage "${validation-source}/validation.nix" {};
  }
