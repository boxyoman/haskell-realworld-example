{ system ? builtins.currentSystem } :
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: {
            realworld = pkgs.haskell.lib.justStaticExecutables
              (haskellPackagesNew.callCabal2nix "realworld" ./. {});
            beam-postgres = pkgs.haskell.lib.dontCheck
              ( haskellPackagesOld.beam-postgres );
          };
        };
    };
  };

  pkgs = import ./pkgs.nix { inherit config system; };

in
  pkgs
