let
  system = "x86_64-linux";
  api = import ./default.nix { inherit system; };
  migrations = ./migrations;
  pkgs = import ./pkgs.nix { inherit system; };

  vbox =
    { ... } :
    { deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 512;
      deployment.virtualbox.headless = true;
      nixpkgs.system = "x86_64-linux";

      networking.firewall.allowedTCPPorts = [ 22 5432 8080 ];
      systemd.services.api = {
        description = "api-server";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          WorkingDirectory = "${api}";
          ExecStart = "${api}/bin/realworld-exe";
          Restart = "always";
          Environment = [ "MIGRATIONS_PATH=${migrations}" ];
        };
      };
      services.postgresql =
        { enable = true;
          package = pkgs.postgresql;
          enableTCPIP = true;
          authentication = ''
            local all all                trust
            host  all all 127.0.0.1/32   trust
            host  all all ::1/128        trust
          '';
          initialScript = pkgs.writeText "backend-initScript" ''
            CREATE ROLE realworld WITH LOGIN PASSWORD 'realworld' CREATEDB;
            CREATE DATABASE realworld;
            GRANT ALL PRIVILEGES ON DATABASE realworld TO realworld;
          '';
        };
    };

in
  { network.description = "nixed";
    network.enableRollback = true;

    backend = vbox;
  }
