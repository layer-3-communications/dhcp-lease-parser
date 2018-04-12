{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, torsor, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/chronos.git";
    sha256 = "0pfv610jfixv7mnarm2qm6abfv0i5v7y11xvij73bvchwk3514c5";
    rev = "85230a0a572d350a4538bb6856d6d7a4862cacde";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring clock hashable primitive text
    torsor vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
