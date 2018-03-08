{ mkDerivation, base, bytestring, fetchgit, gauge, ghc-prim
, hedgehog, stdenv, text
}:
mkDerivation {
  pname = "bytestring-encodings";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/bytestring-encodings.git";
    sha256 = "1r20mxga7m8j0i7gi0d1a84c2rxbvh8a92vz2wspwif9ra29krp3";
    rev = "269d49299f4f1af3fed4ddf5cf49529d237ccfc2";
  };
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  testHaskellDepends = [ base bytestring hedgehog ];
  benchmarkHaskellDepends = [ base bytestring gauge text ];
  description = "checks to see if a given bytestring adheres to a certain encoding";
  license = stdenv.lib.licenses.mit;
}
