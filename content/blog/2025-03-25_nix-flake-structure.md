+++
title = "Nix flake structure"
date = 2025-03-25
draft = false
path = "2025-03/nix-flake-structure"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos"]
+++

On my previous log, I have discussed my [NAS/homelab migration](@/blog/2025-03-18_nas-migration.md).

Among all the services I host, many (as an example, I'll use [withings-weights](https://github.com/blackheaven/withings-weights))
are run using Docker (Podman for NixOS, to be precise).

I took a look at the closure size with [nix-tree](https://github.com/utdemir/nix-tree),
and I have realized that, enabling Docker/Podman was adding many derivations,
which ends up representing 60% of the closure total weight.

Moreover, it's a pain to maintain: set up a GitHub Containers Registry,
maintain a proper CI, either tag the image, or connect to my homelab to run
`docker pull` and restart the service.

So, I have decided to write a proper NixOS service.

Currently, I rely on my nix flake OCI/Docker image definition:

```nix
packages.withings-weights =
  (haskellPackages.callCabal2nix "withings-weights" ./. {}).overrideAttrs (old: {
      postInstall = (old.postInstall or "") + ''
        mkdir -p $out/share/assets
        cp -dr "${assets}/assets" $out/share/
      '';
    });
packages.withings-weights-image = pkgs.dockerTools.buildImage {
  name = "blackheaven/withings-weights";
  tag = "latest";

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths =
      [ pkgs.cacert self.packages.${system}.withings-weights ];
    pathsToLink = [ "/bin" "/etc" "/assets" ];
  };
  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mkdir -p /store
    # Default assets
    cp -dr "${self.packages.${system}.withings-weights}/share/assets" /
  '';
  config = {
    Entrypoint = [ "/bin/withings-weights" ];
    Env = [
      "OAUTH_STORE_PATH=/store/users.json"
      "SERVER_PORT=80"
      "SERVER_ASSETS_PATH=/assets"
    ];
    Volumes = {
      "/store" = { };
      "/assets" = { };
    };
  };
};

packages.default = packages.withings-weights;
```

Hopefully, nix flake comes with a [comprehensive output scheme](https://nixos.wiki/wiki/Flakes#Output_schema):

```nix
{ self, ... }@inputs:
{
  # ...
  # Overlay, consumed by other flakes
  overlays."<name>" = final: prev: { };
  # Default overlay
  overlays.default = final: prev: { };
  # Nixos module, consumed by other flakes
  nixosModules."<name>" = { config, ... }: { options = {}; config = {}; };
  # Default module
  nixosModules.default = { config, ... }: { options = {}; config = {}; };
}
```

So, I was first able to define the overlay:

```nix
let
  # ...
  nixpkgsOverlay = _final: _prev: {
    withings-weights = self.packages.${system}.withings-weights;
  };
in {
  # ...
  overlays = nixpkgsOverlay;
}
```

And then, the service itself:

```nix
nixosModules.default =
  { pkgs, lib, config, ... }:
  let
    cfg = config.services.withings-weights;
    defaultStoreRootPath = "/var/lib/withings-weights";
    defaultStorePath = "${defaultStoreRootPath}/users.json";
    defaultUser = "withings-weights";
    defaultGroup = "withings-weights";
  in
  {
    options = with lib; {
        services.withings-weights = {
          enable = mkEnableOption "Simple withings weight stats WebUI";
          package = lib.mkPackageOption pkgs "withings-weights" {};
          assets = lib.mkOption {
            type = types.path;
            default = "${cfg.package}/share/assets";
          };
          store = lib.mkOption {
            type = types.path;
            default = defaultStorePath;
          };
          oauthCallbackUrl = lib.mkOption {
            type = types.str;
          };
          oauthClientIdFile = lib.mkOption {
            type = types.path;
          };
          oauthClientSecretFile = lib.mkOption {
            type = types.path;
          };
          user = mkOption {
            type = types.str;
            default = defaultUser;
          };
          group = mkOption {
            type = types.str;
            default = defaultGroup;
          };
          openFirewall = lib.mkOption {
            type = types.bool;
            default = false;
          };
          port = lib.mkOption {
            type = types.port;
            default = 5555;
          };
      };
    };
    config = lib.mkIf cfg.enable {
      nixpkgs.overlays = [ nixpkgsOverlay ];

      networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.port;

      users.users = lib.mkIf (cfg.user == defaultUser) {
        withings-weights = {
          isSystemUser = true;
          group = cfg.group;
        };
      };

      users.groups = lib.mkIf (cfg.group == defaultGroup) {
        withings-weights = {};
      };

      systemd.tmpfiles.rules = lib.mkIf (cfg.store == defaultStorePath) [
        "d ${defaultStoreRootPath} 0755 ${cfg.user} ${cfg.group} -"
      ];

      systemd.services.withings-weights = {
        description = "Withings Weights WebUI";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        script = ''
          export OAUTH_CLIENT_ID=$(cat ${cfg.oauthClientIdFile})
          export OAUTH_CLIENT_SECRET=$(cat ${cfg.oauthClientSecretFile})
          exec ${lib.getExe cfg.package}
        '';
        serviceConfig = {
          User = cfg.user;
          Group = cfg.group;
          Environment = [
            "SERVER_PORT=${toString cfg.port}"
            "SERVER_ASSETS_PATH=${cfg.assets}"
            "OAUTH_CALLBACK_URL=${cfg.oauthCallbackUrl}"
            "OAUTH_STORE_PATH=${cfg.store}"
          ];
        };
      };
    };
  };
```

Then, I have to reference it in my NixOS configuration:

```nix
nixosConfigurations = {
  hannibal = inputs.nixpkgs.lib.nixosSystem {
    inherit system;
    modules = [
      ./homelab_hannibal/configuration.nix
      inputs.withings-weights.nixosModules.${system}.default
    ];
  };
};
```

So I can get rid of Docker, and use a regular service:

```nix
services.withings-weights = {
  enable = true;
  oauthClientIdFile = config.age.secrets.withingsOauthClientId.path;
  oauthClientSecretFile = config.age.secrets.withingsOauthClientSecret.path;
};
```
