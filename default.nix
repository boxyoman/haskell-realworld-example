{ system ? builtins.currentSystem } :
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: {
            sked-api = pkgs.haskell.lib.justStaticExecutables
              ( haskellPackagesNew.callPackage ./realworld.nix { });
            beam-core =
              ( haskellPackagesNew.callPackage ./nix/beam-core.nix { } );
            beam-migrate =
              ( haskellPackagesNew.callPackage ./nix/beam-migrate.nix { } );
            beam-postgres =
              ( haskellPackagesNew.callPackage ./nix/beam-postgres.nix { } );
            generic-lens = pkgs.haskell.lib.dontCheck
              ( haskellPackagesNew.callPackage ./nix/generic-lens.nix { } );
            inspection-testing =
              ( haskellPackagesNew.callPackage ./nix/inspection-testing.nix { } );
          };
        };
    };
  };

  pkgs = import ./pkgs.nix { inherit config system; };

in
  pkgs.haskellPackages.sked-api
