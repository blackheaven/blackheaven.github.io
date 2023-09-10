+++
title = "Homelab: Monitoring dashboards"
date = 2023-09-10
draft = false
path = "2023-09/homelab-monitoring-dashboards"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "monitoring", "lgtm-stack"]
+++

Even though I'm not a huge fan of [Web/GUI](@/2023-09-06_homelab-monitoring-introduction.md),
setting up dashboards can help discoverability and debugging (especially if your alerts where not properly set).

While I could set up a basic dashboard, Grafana has a [community-driven catalog](https://grafana.com/grafana/dashboards/).

First, we have to export a bit more metrics:

```nix
services.prometheus.exporters.node.enabledCollectors = [
      "arp"
      "cpu"
      "diskstats"
      "ethtool"
      "filesystem"
      "hwmon"
      "netdev"
      "sysctl"
      "systemd"
    ];
```

Then, we can import few interesting dashboards:

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

Quite basic but it gives a historically-backed view of _Barracuda_.

Next time we'll see how to set up alerting to wrap-up.
