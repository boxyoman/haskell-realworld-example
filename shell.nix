let
  pkgs = (import ./. {});
in
  pkgs.haskellPackages.shellFor {
    nativeBuildInputs = [
      pkgs.haskell-language-server
      pkgs.haskellPackages.cabal-plan
      pkgs.haskellPackages.cabal-install
      pkgs.devenv
    ];
    buildInputs = [ ];
    packages = p : [
      p.realworld
    ];
    withHoogle = true;
  }
