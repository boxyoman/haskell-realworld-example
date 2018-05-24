{ mkDerivation, aeson, base, beam-core, bytestring, containers
, deepseq, dependent-map, dependent-sum, free, ghc-prim, hashable
, haskell-src-exts, mtl, parallel, pqueue, pretty, scientific
, stdenv, text, time, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "beam-migrate";
  version = "0.3.2.0";
  sha256 = "0ee6121f57f67e60db32543be95085220ea1c6256784e878e5b40b75d911d5a8";
  libraryHaskellDepends = [
    aeson base beam-core bytestring containers deepseq dependent-map
    dependent-sum free ghc-prim hashable haskell-src-exts mtl parallel
    pqueue pretty scientific text time unordered-containers uuid-types
    vector
  ];
  homepage = "https://travis.athougies.net/projects/beam.html";
  description = "SQL DDL support and migrations support library for Beam";
  license = stdenv.lib.licenses.mit;
}
