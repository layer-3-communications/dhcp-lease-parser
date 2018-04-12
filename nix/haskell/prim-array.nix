{ mkDerivation, base, fetchgit, ghc-prim, primitive, semigroups
, stdenv
}:
mkDerivation {
  pname = "prim-array";
  version = "0.2.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/prim-array.git";
    sha256 = "0lyx56njigwx0pw7g7wq4sfmaind3kp61mdinjx3g9k74dmj2yyy";
    rev = "c4b722ab04f14784f17fe013dab19b441c511f79";
  };
  libraryHaskellDepends = [ base ghc-prim primitive semigroups ];
  homepage = "https://github.com/andrewthad/prim-array#readme";
  description = "Primitive byte array with type variable";
  license = stdenv.lib.licenses.bsd3;
}
