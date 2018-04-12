{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, hashable, hspec, HUnit, primitive, QuickCheck
, quickcheck-classes, semigroups, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/haskell-ip.git";
    sha256 = "0jq5ir21brcldbz6fv7dag3df2zf4ixbfzlh0sbalipp9z282zp1";
    rev = "0a9d76d491ad814109b230f9eb38d1c85c795127";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive semigroups text
    vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring doctest hspec HUnit QuickCheck
    quickcheck-classes test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion text
  ];
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  license = stdenv.lib.licenses.bsd3;
}
