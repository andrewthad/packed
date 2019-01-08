{ mkDerivation, base, fetchgit, primitive, QuickCheck
, quickcheck-classes, stdenv, tagged, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "primitive-maybe";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-maybe.git";
    sha256 = "1w06cwm3qra0ygm9x4xc48nwxma2ipbnqqml28dx0fk4kjqn5lpm";
    rev = "8e0aa60636fb261ff4d2daf853532319d403212a";
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
