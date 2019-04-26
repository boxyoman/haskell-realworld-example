(import ./. {}).pkgs.haskellPackages.shellFor {
  packages = p :
    [ p.realworld
    ];
  withHoogle = true;
}

