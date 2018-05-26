{ mkDerivation, aeson, base-noprelude, beam-core, beam-postgres
, bytestring, classy-prelude, conduit, containers, cryptonite
, generic-lens, hspec, hspec-wai, hspec-wai-json, jose, lens
, monad-control, mtl, postgresql-simple
, postgresql-simple-migration, resource-pool, rio, servant-server
, stdenv, text, time, transformers, vector, wai, wai-cors
, wai-extra, wai-logger, warp
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
    monad-control mtl postgresql-simple resource-pool rio
    servant-server text time transformers vector wai warp
  ];
  executableHaskellDepends = [
    base-noprelude classy-prelude postgresql-simple
    postgresql-simple-migration wai wai-cors wai-extra wai-logger warp
  ];
  testHaskellDepends = [
    aeson base-noprelude classy-prelude hspec hspec-wai hspec-wai-json
  ];
  homepage = "https://github.com/githubuser/realworld#readme";
  license = stdenv.lib.licenses.bsd3;
}
