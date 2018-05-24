{ mkDerivation, base, containers, ghc, mtl, stdenv
, template-haskell, transformers
}:
mkDerivation {
  pname = "inspection-testing";
  version = "0.2.0.1";
  sha256 = "1f699bf8e95ab90d36725a8a090ad052dbb051cce379fd45a664f561e66ea194";
  libraryHaskellDepends = [
    base containers ghc mtl template-haskell transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/nomeata/inspection-testing";
  description = "GHC plugin to do inspection testing";
  license = stdenv.lib.licenses.mit;
}
