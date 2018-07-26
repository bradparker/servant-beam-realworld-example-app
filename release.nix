{ nixpkgs ? import ./nix/nixpkgs.nix {}
, tag ? "latest"
}:
nixpkgs.dockerTools.buildImage {
  name = "realworld-conduit";
  tag = tag;
  contents = import ./. { check = false; };
  config = {
    Cmd = ["/bin/realworld-conduit"];
  };
}
