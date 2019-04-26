a :
let
  inherit (import <nixpkgs> a) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      rev = "571b40d3f50466d3e91c1e609d372de96d782793";
      sha256 = "0qjpkx2b5pac1nqr5chvlrlcyyk294w2079ybf88p95qxkqbbby5";
    };
in
  import nixpkgs a
