{ lib
, fetchFromGitHub
, ...
}:
self:
super:
let
  base-noprelude-source = fetchFromGitHub {
    owner = "haskell-hvr";
    repo = "base-noprelude";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    base-noprelude = self.callPackage (self.haskellSrc2nix {
      name = "base-noprelude";
      src = base-noprelude-source;
    }) {};
  }
