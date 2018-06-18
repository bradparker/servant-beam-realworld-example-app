with import <nixpkgs> {};
config:
import (fetchFromGitHub {
  owner = "NixOs";
  repo = "NixPkgs";
  inherit (builtins.fromJSON (builtins.readFile ./nixpkgs.json)) rev sha256;
}) config
