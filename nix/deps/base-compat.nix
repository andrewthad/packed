{ mkDerivation, base, fetchgit, stdenv, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.10.4";
  src = fetchgit {
    url = "https://github.com/chessai/base-compat";
    sha256 = "0wviml7s34phpsvqh91w0q5sh9r94fv4zja4x86n6qivrp84zga1";
    rev = "03fed2390a30577603cd4bae69be79340e815f15";
  };
  postUnpack = "sourceRoot+=/base-compat; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base unix ];
  description = "A compatibility layer for base";
  license = stdenv.lib.licenses.mit;
}
