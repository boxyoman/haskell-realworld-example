{ system ? builtins.currentSystem } :
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: {
            realworld = pkgs.haskell.lib.justStaticExecutables
              ( haskellPackagesNew.callPackage ./realworld.nix { });
          };
        };
    };
  };

  pkgs = import ./pkgs.nix { inherit config system; };

in
  {
    pkgs = pkgs;
    realworkd = pkgs.haskellPackages.realworld;
  }
