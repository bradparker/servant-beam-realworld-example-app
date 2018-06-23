with import <nixpkgs> {};
config:
import (fetchFromGitHub {
  owner = "NixOs";
  repo = "NixPkgs";
  rev = lib.fileContents ./nixpkgs-rev.txt;
  sha256 = lib.fileContents  ./nixpkgs-sha.txt;
}) config
