+++
title = "Announcing grafana-dashboards.nix"
date = 2023-09-20
draft = false
path = "2023-09/announcing-grafana-dashboard-nix"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "monitoring", "lgtm-stack"]
+++

Few logs ago, I introduced [dashboards](@/2023-09-10_homelab-monitoring-dashboards.md) I borrowed online.

I wrote they were looking like that:

```nix
services.grafana.provision.dashboards.settings.providers =
  let
    fetchDashboard = { name, hash, id, version }:
      pkgs.fetchurl {
        inherit name hash;
        url = "https://grafana.com/api/dashboards/${toString id}/revisions/${toString version}/download";
        recursiveHash = true;
        postFetch = ''
          mv "$out" temp
          mkdir -p "$out"
          mv temp "$out/${name}.json";
        '';
      };
    dashboard = name: fetchArgs: { inherit name; options.path = fetchDashboard fetchArgs; };
  in
  [
    (dashboard "Node Exporter Full"
      {
        name = "node-exporter-full";
        hash = "sha256-ZiIsNaxPE5skpDykcugveAa3S8sCjR9bA9hbzyz7kvY=";
        id = 1860;
        version = 32;
      })
    (dashboard "Node Exporter"
      {
        name = "node-exporter";
        hash = "sha256-2xgE0m3SUFiux501uCVb4aH3zGfapW/SmfxRsFC/514=";
        id = 13978;
        version = 2;
      })
    (dashboard "Docker"
      {
        name = "docker";
        hash = "sha256-RdTcQxdBvbgKGrlhQB1obLrrJENcuRgZ92ZVj06Oiww=";
        id = 10619;
        version = 1;
      })
    (dashboard "Loki Stack"
      {
        name = "loki";
        hash = "sha256-9kM8MrXuL0TD2Z1Uhs4Lp0mNAazLnVfZZk5argdSoRU=";
        id = 14055;
        version = 5;
      })
    (dashboard "AlertManager"
      {
        name = "alertmanager";
        hash = "sha256-Yvw0DGQJpqBYNzE4ES/x7ZAYF7iJ4SUNBKB+sJRuGBw=";
        id = 9578;
        version = 4;
      })
    (dashboard "Restic"
      {
        name = "restic";
        hash = "sha256-XDYT2VAJQ97rRO3kIysH7X980YCFhyEwr2rOMmHqljg=";
        id = 17554;
        version = 1;
      })
  ];
```

But it was not the truth, actually, some of them were not working out-of-the box.

I had to download them, edit them (adding templating inputs, etc.), add the giant blob of json to git.

It not acceptable, not only it's kind of dirty, I completely loose track of the semantic of the changes, making upgrades painful.

That's one of the reason I have just create [grafana-dashboards.nix](http://github.com/blackheaven/grafana-dashboards.nix),
which aims to expression dashboards transformations, such as:

Filling a template variable:

```nix
let
  raw =
    lib.fetchDashboard {
      name = "node-exporter-full";
      hash = "sha256-ZiIsNaxPE5skpDykcugveAa3S8sCjR9bA9hbzyz7kvY=";
      id = 1860;
      version = 32;
    };
in
lib.saveDashboard {
  name = "saved-node-explorer-full";
  path =
    lib.changePath {
      name = "final-dashboard-node-explorer-full";
      path = raw;
      transformations = lib.fillTemplating "job" "nodes";
    };
};
```

Or adding a template variable

```nix
let
  raw =
    lib.fetchDashboard {
      name = "restic-exporter";
      hash = "sha256-HuN1YSR51mf6F56BDLYUOD1LaP5F2kae897y+uL9mwk=";
      id = 17554;
      version = 1;
    };
in
lib.saveDashboard {
  name = "saved-restic-exporter";
  path =
    lib.changePath {
      name = "final-dashboard-restic-exporter";
      path = raw;
      transformations = lib.prependTemplatings [lib.templatingJob];
    };
};
```

Which changes my dashboards definition to:

```nix
services.grafana.provision.dashboards.settings.providers =
  let
    fetchDashboard = { name, hash, id, version, dashboardTitle, transformations }@args:
      with grafanaDashboardsLib;
      saveDashboard
        {
          inherit name;
          path = changePath {
            name = "transformed-dashboard-${name}";
            path = grafanaDashboardsLib.fetchDashboard { inherit name id version hash; };
            transformations = original:
              let
                final = if args.transformations == null then (x: x) else args.transformations;
                regular = x:
                  builtins.foldl' (acc: f: f acc) x [
                    (fillTemplating "DS_PROMETHEUS" prometheusGrafanaDataSourceUid)
                    (setTitle dashboardTitle)
                  ];
              in
              final (regular original);
          };
        };
    dashboard = dashboardTitle: fetchArgs: {
      name = dashboardTitle;
      options.path = fetchDashboard ({ inherit dashboardTitle; transformations = null; } // fetchArgs);
    };
  in
  [
    (dashboard "Host usage"
      {
        # https://grafana.com/grafana/dashboards/1860-node-exporter-full/
        name = "node-exporter-full";
        hash = "sha256-j7+JnG88/eU2xYahjz6J2mWE4w5dRGwvRGrpqp9hPjA=";
        id = 1860;
        version = 32;
        transformations = x:
          (grafanaDashboardsLib.fillTemplating "job" "nodes"
            (grafanaDashboardsLib.fillTemplating "node" "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" x));
      })
    (dashboard "Docker containers"
      {
        # https://grafana.com/grafana/dashboards/10619-docker-host-container-overview/
        name = "docker";
        hash = "sha256-FQThxcxrGtTzycsx2IsCq0ifXdXXR5//nuu+GbjAQro=";
        id = 10619;
        version = 1;
        transformations = x:
          grafanaDashboardsLib.fillTemplating "port" (toString config.services.cadvisor.port)
            (grafanaDashboardsLib.fillTemplating "node" "127.0.0.1"
              (grafanaDashboardsLib.fillTemplating "job" "advisor" x));
      })
    (dashboard "AlertManager"
      {
        name = "alertmanager";
        hash = "sha256-jOINQJbKhaBSntco+3XP9n7l1XDYQy5iZ+TbKPfseCM=";
        id = 9578;
        version = 4;
        transformations = x:
          # grafanaDashboardsLib.fillTemplating "instance" "127.0.0.1:${toString config.services.prometheus.alertmanager.port}"
          (grafanaDashboardsLib.fillTemplating "datasource" prometheusGrafanaDataSourceUid x);
      })
    (dashboard "Restic"
      {
        # https://github.com/ngosang/restic-exporter
        # https://grafana.com/grafana/dashboards/17554-restic-exporter/
        name = "restic";
        hash = "sha256-HuN1YSR51mf6F56BDLYUOD1LaP5F2kae897y+uL9mwk=";
        id = 17554;
        version = 1;
      })
  ];
```

In my case, I hard-code some parameters as I have a static, single-node infrastructure.
