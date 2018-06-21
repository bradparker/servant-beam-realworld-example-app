with import <nixpkgs> {};
{ packages
}:
let
  validation-source = fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    inherit (builtins.fromJSON (builtins.readFile ./validation.json)) rev sha256;
  };
in
  {
    validation = packages.callPackage "${validation-source}/validation.nix" {};
  }
