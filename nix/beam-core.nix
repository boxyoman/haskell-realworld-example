{ mkDerivation, aeson, base, bytestring, containers, dlist, free
, ghc-prim, hashable, microlens, mtl, network-uri, stdenv, tagged
, tasty, tasty-hunit, text, time, vector-sized
}:
mkDerivation {
  pname = "beam-core";
  version = "0.7.2.1";
  sha256 = "5c8e94dab94248189a53e89d515e696c882ffabb91b777c7b1e59d3cf2d5356d";
  libraryHaskellDepends = [
    aeson base bytestring containers dlist free ghc-prim hashable
    microlens mtl network-uri tagged text time vector-sized
  ];
  testHaskellDepends = [
    base bytestring tasty tasty-hunit text time
  ];
  homepage = "http://travis.athougies.net/projects/beam.html";
  description = "Type-safe, feature-complete SQL query and manipulation interface for Haskell";
  license = stdenv.lib.licenses.mit;
}
