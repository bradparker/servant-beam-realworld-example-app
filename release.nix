let
  nixpkgs = import ./nix/packages;
  package = nixpkgs.haskell.lib.justStaticExecutables
    (nixpkgs.haskell.lib.dontCheck
      (import ./.));
in
  nixpkgs.dockerTools.buildImage {
    name = "realworld-conduit";
    tag = "latest";
    contents = [
      nixpkgs.busybox
      package
    ];
    config = {
      Cmd = ["/bin/realworld-conduit"];
    };
  }
