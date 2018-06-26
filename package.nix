{ mkDerivation, aeson, base, beam-core, beam-postgres, bytestring
, hspec, optparse-applicative, postgresql-simple, scrypt, servant
, servant-server, stdenv, text, transformers, validation, wai
, wai-logger, warp
}:
mkDerivation {
  pname = "realworld-conduit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-postgres bytestring optparse-applicative
    postgresql-simple scrypt servant servant-server text transformers
    validation wai wai-logger warp
  ];
  executableHaskellDepends = [
    aeson base beam-core beam-postgres bytestring optparse-applicative
    postgresql-simple scrypt servant servant-server text transformers
    validation wai wai-logger warp
  ];
  testHaskellDepends = [
    aeson base beam-core beam-postgres bytestring hspec
    optparse-applicative postgresql-simple scrypt servant
    servant-server text transformers validation wai wai-logger warp
  ];
  description = "Exemplary fullstack Medium.com clone powered by Servant and Beam";
  license = stdenv.lib.licenses.bsd3;
}
