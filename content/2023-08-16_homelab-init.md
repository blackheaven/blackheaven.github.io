+++
title = "Homelab: current state and plan"
date = 2023-08-16
draft = false
path = "2023-08/homelab-init"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos"]
+++

My home network is composed of 4 machines:

* _Looping_
  * Kind: a desktop computer, custom-built
  * In service from: 2020/10
  * Usage frequency: Daily
  * Usage: coding, writing, working
* _Faceman_
  * Kind: Laptop, Lenovo ThinkPad T440
  * In service from: 2014/02
  * Usage frequency: Rarely
  * Usage: conferences, in-person coding dojos, travels (whenever I dare go out of my home)
* _Hannibal_
  * Kind: NAS, Netgear ReadyNAS 104
  * In service from: 2014/05
  * Usage frequency: 24/7
  * Usage: Store my huge/long-term data (administrative documents, travel pictures, podcasts, snapshots/backups)
* _Barracuda_
  * Kind: Mini-PC, MINIS FORUM GK41
  * In service from: 2022/07
  * Usage frequency: 24/7
  * Usage: hosts my services, [ZNC](https://wiki.znc.in/ZNC) (IRC bouncer), backups base (through [Restic](https://restic.net/))

(Yes, they are all named after [The A-Team tv series](https://en.wikipedia.org/wiki/The_A-Team) characters' nickname)

Except _Hannibal_, they are all running NixOS, but I do manage them one by one
(each of them having their own `/etc/nixos/configuration.nix`, forcing me the login/ssh), which has many drawbacks:

* Except _Looping_, which is updated every Friday, they are real out-dated (I update _Faceman_ before each departure, and _Barracuda_ was updated twice)
* No big picture regarding my global infrastructure
* Divergent configurations

My aim is simple:

* regroup all my infrastructure configurations in my already existing git repository (which only contains my _Looping_ configuration)
* secure my backups and services accesses (which are accessible via HTTP, without authentication)
* automate updates
* set up monitoring and alerting
* set up a DNS to avoid copy-pasting records everywhere

I have chosen [deploy-rs](https://github.com/serokell/deploy-rs), which is a bit too complex for my needs, but I use it at work.

Let's start with the `flake.nix`:

```nix
{
  description = "Black's deployments";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = { self, nixpkgs, deploy-rs }:
    let
      system = "x86_64-linux";
      # Unmodified nixpkgs
      pkgs = import nixpkgs { inherit system; };
      # nixpkgs with deploy-rs overlay but force the nixpkgs package
      deployPkgs = import nixpkgs {
        inherit system;
        overlays = [
          deploy-rs.overlay
          (self: super: { deploy-rs = { inherit (pkgs) deploy-rs; lib = super.deploy-rs.lib; }; })
        ];
      };
    in
    rec
    {
      nixosConfigurations = {
        barracuda = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [ ./barracuda/configuration.nix ];
        };
      };

      deploy = {
        nodes = {
          barracuda = {
            hostname = "***";
            sshUser = "***";
            remoteBuild = false;
            profiles.system = {
              user = "root";
              path = deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.barracuda;
              # path = deployPkgs.deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.bare;
            };
          };
        };
      };

      # This is highly advised, and will prevent many possible mistakes
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      devShells.${system}.default =
        let
          scripts = pkgs.symlinkJoin {
            name = "scripts";
            paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin { };
          };
        in
        pkgs.mkShell {
          buildInputs = [
            scripts
            pkgs.deploy-rs
            pkgs.dhall
            pkgs.libressl
          ];
          inputsFrom = [
            # self.defaultPackage.${system}.env
          ];
        };
    };
}
```

It's mostly copy-pasted from their `README`.

Then, some parts of my `barracuda/configuration.nix`:

```nix
{ config, pkgs, lib, ... }:

{
  nix = {
    settings = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  imports = [ ./hardware-configuration.nix ];

  virtualisation.docker.enable = true;

  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
    };
  };
  # NFS mount
  services.rpcbind.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [
    80 # nignx
    443 # nignx
    6501 # irc/znc
  ];

  systemd.mounts =
    let
      commonMountOptions = {
        type = "nfs";
        mountConfig = {
          Options = "noatime,user,rw,nofail";
        };
      };
      mountPoint = mountFolder: mountShare:
        (commonMountOptions // {
          what = "xxx:/MainVol/${mountShare}";
          where = "/mnt/${mountFolder}";
        });

    in

    [
      (mountPoint "hannibal_medias" "medias")
    ];

  systemd.automounts =
    let
      commonAutoMountOptions = {
        wantedBy = [ "multi-user.target" ];
        automountConfig = {
          TimeoutIdleSec = "0"; # never
        };
      };
      onMnt = folder: (commonAutoMountOptions // { where = "/mnt/${folder}"; });

    in

    [
      (onMnt "hannibal_medias")
    ];

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "restic.barracuda.local" = {
        enableACME = false;
        # addSSL = true;
        serverAliases = [ ];
        locations."/" = {
          proxyPass = "http://127.0.0.1:8484/";
        };
      };
      "irc.barracuda.local" = {
        enableACME = false;
        serverAliases = [ ];
        locations."/" = {
          proxyPass = "http://127.0.0.1:6501/";
        };
      };
      "withings.barracuda.local" = {
        enableACME = false;
        serverAliases = [ ];
        locations."/" = {
          proxyPass = "http://127.0.0.1:5555/";
        };
      };
    };
  };

  virtualisation.oci-containers.containers = {
    withings = {
      image = "ghcr.io/blackheaven/withings-weights:latest";
      dependsOn = [ ];
      environment = {
        # ...
      };
      ports = [
        # host:container
        "5555:80"
      ];
      volumes = [
        "/mnt/hannibal_medias/withings:/store"
      ];
    };
    znc = {
      image = "lscr.io/linuxserver/znc:latest";
      environment = {
        PUID = "1000";
        PGID = "1000";
        TZ = "Europe/Paris";
      };
      ports = [
        # host:container
        "6501:6501"
      ];
      volumes = [
        "/mnt/hannibal_medias/znc:/config"
      ];
    };
  };

  systemd.services.podman-znc = {
    after = [ "mnt-hannibal-medis.mount" ];
  };


  services.restic.server = {
    enable = true;
    appendOnly = true;
    dataDir = "/mnt/hannibal_medias/backup";
    prometheus = false;
    extraFlags = [ "--no-auth" ]; # auth is done via firewall (lol)
    listenAddress = ":8484";
  };
  systemd.services.restic-rest-server = {
    after = [ "mnt-hannibal-medis.mount" ];
  };
}
```

On looping, I have few configuration, from restic:

```nix
  services.restic.backups = {
    nixosConfig = {
      user = "root";
      repository = "rest:http://restic.barracuda.local/looping_nixosConfig/";
      passwordFile = "...";
      extraBackupArgs = [ "" ];
      paths = [ "/etc/nixos" ];
      timerConfig = {
        OnCalendar = "09:45";
      };
    };
    home = {
      user = "xxx";
      repository = "rest:http://restic.barracuda.local/looping_home/";
      passwordFile = "...";
      extraBackupArgs = [ "" ];
      paths = [ "/home/xxx" ];
      exclude = [
        # ...
      ];
      timerConfig = {
        OnCalendar = "Fri *-*-* 15:00:00";
      };
    };
  };
```

Quite neat, but the thing I'd like to get rid of is my `/etc/hosts`:

```nix
192.168.0.4 withings.barracuda.local
192.168.0.4 irc.barracuda.local
192.168.0.4 restic.barracuda.local
```
