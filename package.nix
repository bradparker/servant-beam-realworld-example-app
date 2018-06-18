{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "realworld-conduit";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  description = "Exemplary fullstack Medium.com clone powered by Servant and Beam";
  license = stdenv.lib.licenses.bsd3;
}
