{ }:

with rec {
  pkgs = (import ./nix/nixpkgs.nix {});
  drv = pkgs.haskell.packages.new-ghcHEAD.packed;
};

drv
