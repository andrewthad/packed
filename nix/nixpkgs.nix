{ }:

with rec {
  fetchNixpkgs = import ./fetchNixpkgs.nix;

  _nixpkgs = fetchNixpkgs {
    rev = "d767ba99964eb776c2b482ca8a3a0ef42d7ccf8b";
    sha256 = "0k7nh9m0wkiyszal4fywj57j4zx2yzhzgq1182qn937jg9fa30gl"; 
  };

};

import _nixpkgs {
  config = { };
  overlays = [ (import ./new-ghcHEAD.nix) ];
}
