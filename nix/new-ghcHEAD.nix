let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  nixpkgs = import (fetchNixpkgs {
    rev = "d767ba99964eb776c2b482ca8a3a0ef42d7ccf8b";
    sha256 = "0k7nh9m0wkiyszal4fywj57j4zx2yzhzgq1182qn937jg9fa30gl";
  }) { config = {}; overlays = []; };

  patchRepo = nixpkgs.fetchFromGitHub {
    owner = "chessai";
    repo  = "head.hackage";
    rev   = "6b054a4b2cb17fea14a459959ead75d41103f219";
    sha256 = "1z0sfm1rk6xwb8bni8gl2wv2g48j4bj1bb76yhc1hcgp8sz67gda";
  };

  patchDir = "${patchRepo}/patches";
  patchScript = "${patchRepo}/scripts/overrides.nix";

in

self: super:
{
  patches = super.callPackage patchScript { patches = patchDir; };

  haskell = super.haskell // {
    packages = super.haskell.packages // {
      new-ghcHEAD = let ghcPackageOverrides = super.callPackage self.patches {};
        localOverrides = sel: sup: {
          mkDerivation = drv: sup.mkDerivation (drv //
            { jailbreak = true;
              doHaddock = false;
              doCheck   = false;
            });

          primitive = sel.callPackage ./deps/primitive.nix {};
          primitive-maybe = sel.callPackage ./deps/primitive-maybe.nix {};

          # source should be cleaned here
          packed = sel.callCabal2nix "packed" ../. {};
        
        };
    in super.haskell.packages.ghcHEAD.extend (self.lib.composeExtensions localOverrides ghcPackageOverrides);
    };

  }; 
  
}