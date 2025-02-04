{ pkgs, ... } :
let
  migrations = ./migrations;
in {
  name = "realworld";
  services.postgres = {
    enable = true;
    package = pkgs.postgresql_15;
    createDatabase = true;
    initialDatabases = [
      { name = "realworld"; }
    ];
    listen_addresses = "127.0.0.1";
    initialScript = ''
      CREATE USER realworld with SUPERUSER password 'realworld';
    '';
  };

  processes.backend = {
    exec = ''
      sleep 1
      export MIGRATIONS_PATH=${migrations}
      nix-shell --command "cabal run"
    '';
    process-compose = {
      depends_on.postgres.condition = "process_started";
    };
  };


  processes.hoogle = {
    exec = ''
      nix-shell --command "hoogle server --local --port 8081"
    '';
  };
}

