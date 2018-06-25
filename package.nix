{ mkDerivation, base, beam-core, beam-postgres, bytestring, hspec
, optparse-applicative, postgresql-simple, scrypt, stdenv, text
, transformers, validation
}:
mkDerivation {
  pname = "realworld-conduit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base beam-core beam-postgres bytestring optparse-applicative
    postgresql-simple scrypt text transformers validation
  ];
  executableHaskellDepends = [
    base beam-core beam-postgres bytestring optparse-applicative
    postgresql-simple scrypt text transformers validation
  ];
  testHaskellDepends = [
    base beam-core beam-postgres bytestring hspec optparse-applicative
    postgresql-simple scrypt text transformers validation
  ];
  description = "Exemplary fullstack Medium.com clone powered by Servant and Beam";
  license = stdenv.lib.licenses.bsd3;
}
