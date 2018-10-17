{ mkDerivation, base, fetchgit, primitive, QuickCheck
, quickcheck-classes, stdenv, tagged, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "primitive-maybe";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-maybe.git";
    sha256 = "0gb8wzvvas8dfxf3pryjcd6awb20b5ngqsll5hqkqqjnmb7jglqz";
    rev = "8f573a758ddc38b4fce1cabce898d3cc58249cb3";
  };
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [
    base primitive QuickCheck quickcheck-classes tagged tasty
    tasty-quickcheck
  ];
  homepage = "https://github.com/andrewthad/primitive-maybe#readme";
  description = "Arrays of Maybes";
  license = stdenv.lib.licenses.bsd3;
}
