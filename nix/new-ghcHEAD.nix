let
  nixpkgs = import <nixpkgs> {};
  
  patchRepo = nixpkgs.fetchFromGitHub {
    owner = "chessai";
    repo  = "head.hackage";
    rev   = "c157b8c3e661fc5573316006cc109a3b84481803";
    sha256 = "0ijfnjgpl5zc939lrpfqqprayd0mwz8snsba608vkmwhfjwyp07p";
  };

  patchDir = "${patchRepo}/patches";
  patchScript = "${patchRepo}/scripts/overrides.nix";

in

self: super:
{
  patches = super.callPackage patchScript { patches = patchDir; };

  new-ghcHEAD =
    let ghcPackageOverrides = super.callPackage self.patches {};
        localOverrides = sel: sup: {
          mkDerivation = drv: sup.mkDerivation (drv //
            { jailbreak = true;
              doHaddock = false;
            });

          packed = (
            with rec {
              packedSource = sel.lib.cleanSource ../.;
              packedBasic = sel.callCabal2nix "packed" packedSource {};
            };
            overrideCabal packedBasic (old: {
            })
          );
        
        };
    in super.haskell.packages.ghcHEAD.extend (self.lib.composeExtensions localOverrides ghcPackageOverrides);
}
