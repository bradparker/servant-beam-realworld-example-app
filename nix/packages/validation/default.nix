packages:
config:
let
  inherit (import ../nixpkgs config)
    fetchFromGitHub
    lib;

  validation-source = fetchFromGitHub {
    owner = "qfpl";
    repo = "validation";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    validation = packages.callPackage "${validation-source}/validation.nix" config;
  }
