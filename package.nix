{ mkDerivation, aeson, base, beam-core, beam-postgres, bytestring
, containers, data-default, hpack, hspec, hspec-wai, hspec-wai-json
, http-types, insert-ordered-containers, jose, lens, monad-control
, optparse-applicative, postgresql-simple, resource-pool, scrypt
, servant, servant-auth, servant-auth-server, servant-auth-swagger
, servant-server, servant-swagger, servant-swagger-ui
, servant-swagger-ui-core, stdenv, swagger2, text, time
, transformers, validation, wai, wai-extra, warp
}:
mkDerivation {
  pname = "realworld-conduit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-postgres bytestring containers
    data-default http-types insert-ordered-containers jose lens
    monad-control optparse-applicative postgresql-simple resource-pool
    scrypt servant servant-auth servant-auth-server
    servant-auth-swagger servant-server servant-swagger
    servant-swagger-ui servant-swagger-ui-core swagger2 text time
    transformers validation wai wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base beam-core beam-postgres bytestring containers
    data-default http-types insert-ordered-containers jose lens
    monad-control optparse-applicative postgresql-simple resource-pool
    scrypt servant servant-auth servant-auth-server
    servant-auth-swagger servant-server servant-swagger
    servant-swagger-ui servant-swagger-ui-core swagger2 text time
    transformers validation wai wai-extra warp
  ];
  testHaskellDepends = [
    aeson base beam-core beam-postgres bytestring containers
    data-default hspec hspec-wai hspec-wai-json http-types
    insert-ordered-containers jose lens monad-control
    optparse-applicative postgresql-simple resource-pool scrypt servant
    servant-auth servant-auth-server servant-auth-swagger
    servant-server servant-swagger servant-swagger-ui
    servant-swagger-ui-core swagger2 text time transformers validation
    wai wai-extra warp
  ];
  preConfigure = "hpack";
  description = "Exemplary fullstack Medium.com clone powered by Servant and Beam";
  license = stdenv.lib.licenses.bsd3;
}
