{ nixpkgs ? import ./nix/packages/nixpkgs {}
}:
nixpkgs.dockerTools.buildImage {
  name = "realworld-conduit";
  tag = "latest";
  contents = [
    nixpkgs.busybox
    (import ./. { check = false; })
  ];
  config = {
    Cmd = ["/bin/realworld-conduit"];
  };
}
