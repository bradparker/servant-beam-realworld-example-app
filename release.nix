{ nixpkgs ? import ./nix/packages/nixpkgs {}
}:
nixpkgs.dockerTools.buildImage {
  name = "realworld-conduit";
  tag = "latest";
  contents = [
    nixpkgs.busybox
    (nixpkgs.haskell.lib.justStaticExecutables (import ./. { check = false; }))
  ];
  config = {
    Cmd = ["/bin/realworld-conduit"];
  };
}
