{ mkDerivation, aeson, base, base-noprelude, beam-core
, beam-postgres, bytestring, classy-prelude, conduit, containers
, cryptonite, generic-lens, hspec, hspec-wai, hspec-wai-json, jose
, lens, monad-control, postgresql-simple, rio, servant-server
, stdenv, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "realworld";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base-noprelude beam-core beam-postgres bytestring
    classy-prelude conduit containers cryptonite generic-lens jose lens
    monad-control postgresql-simple rio servant-server text time
    transformers wai warp
  ];
  executableHaskellDepends = [ base classy-prelude ];
  testHaskellDepends = [
    aeson base classy-prelude hspec hspec-wai hspec-wai-json
  ];
  homepage = "https://github.com/githubuser/realworld#readme";
  license = stdenv.lib.licenses.bsd3;
}
