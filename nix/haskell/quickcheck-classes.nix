{ mkDerivation, aeson, base, bifunctors, containers, fetchgit
, primitive, QuickCheck, semigroupoids, semigroups, stdenv, tagged
, transformers, vector
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.4.9";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "05bzkh18c05y7qxn1acz7q5xy3ggdsb0lhgzm1qsh5yrng67q86v";
    rev = "77314dcec79ffb7477693bd6eaac4c23995e51e4";
  };
  libraryHaskellDepends = [
    aeson base bifunctors containers primitive QuickCheck semigroupoids
    semigroups tagged transformers
  ];
  testHaskellDepends = [
    aeson base primitive QuickCheck tagged transformers vector
  ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
