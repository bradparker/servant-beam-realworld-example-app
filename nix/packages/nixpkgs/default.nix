config:
let
  inherit (import <nixpkgs> {})
    fetchFromGitHub
    lib;
in
  import (fetchFromGitHub {
    owner = "NixOs";
    repo = "NixPkgs";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  }) config
