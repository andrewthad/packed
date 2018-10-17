{ mkDerivation, base, fetchgit, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.4.0";
  src = fetchgit {
    url = "https://github.com/haskell/primitive.git";
    sha256 = "08zg9yfhcx55qaj722w4q69nxvfh4jhb31g4xyk3zrxxk6agf4nh";
    rev = "b43717b2e86055a7cd94c46e1b77a72a9d773e55";
  };
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
