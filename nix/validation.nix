packages: config:
let
  inherit (import <nixpkgs> {})
    fetchFromGitHub
    lib;

  validation-source = fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    rev = lib.fileContents ./validation/rev;
    sha256 = lib.fileContents  ./validation/sha;
  };
in
  {
    validation = packages.callPackage "${validation-source}/validation.nix" config;
  }
