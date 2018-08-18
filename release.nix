{ nixpkgs ? import ./nix/packages/nixpkgs {}
}:
nixpkgs.dockerTools.buildImage {
  name = "realworld-conduit";
  contents = [
    nixpkgs.busybox
    (import ./. { check = false; })
  ];
  config = {
    Cmd = ["/bin/realworld-conduit"];
  };
}
