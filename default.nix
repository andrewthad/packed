{ }:

let
  pkgs = (import ./nix/nixpkgs.nix {});
  drv = pkgs.haskell.packages.new-ghcHEAD.packed;

in rec {
  inherit drv;
  inherit pkgs;

  packed = drv;
}