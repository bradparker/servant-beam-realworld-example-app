with import <nixpkgs> {};
config:
import (fetchFromGitHub {
  owner = "NixOs";
  repo = "NixPkgs";
  rev = lib.fileContents ./nixpkgs/rev;
  sha256 = lib.fileContents  ./nixpkgs/sha;
}) config
