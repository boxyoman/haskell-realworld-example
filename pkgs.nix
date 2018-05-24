a :
let
  inherit (import <nixpkgs> a) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      rev = "06c576b0525da85f2de86b3c13bb796d6a0c20f6";
      sha256 = "01cra89drfjf3yhii5na0j5ivap2wcs0h8i0xcxrjs946nk4pp5j";
    };
in
  import nixpkgs a
