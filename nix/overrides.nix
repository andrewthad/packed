{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  primitive-maybe = self.callPackage ./deps/primitive-maybe.nix { }; 
  
  packed = (
    with rec {
      packedSource = pkgs.lib.cleanSource ../.;
      packedBasic  = self.callCabal2nix "packed" packedSource { };
    };
    overrideCabal packedBasic (old: {
    })
  );
}
