+++
title = "Homelab: Monitoring alerting"
date = 2023-09-13
draft = false
path = "2023-09/homelab-monitoring-alerting"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "monitoring", "lgtm-stack"]
+++

Previously, we have set few [Dashboards](@/blog/2023-09-10_homelab-monitoring-dashboards.md),
which is great for analysis, but not for reactivity.

I'm not always on my monitoring tab, but I always have an IRC client open.

Sadly, Grafana Alert center does not provide such an integration, we have to go through a Google backend:

```nix
services.prometheus.  alertmanagerIrcRelay = {
  enable = true;
  settings = {
    http_host = "localhost";
    http_port = 3040;

    irc_host = "127.0.0.1";
    irc_port = 6502;
    use_privmsg = true;
    irc_use_ssl = false;
    irc_nickname = "alertmanagerIrcRelayBot";

    irc_channels = [
      { name = "#barracuda-monitoring"; }
    ];
  };
};
```

which creates a user to my local IRC server.

Note: I used `use_privmsg = true`, to emit plain messages (and not just notices) in the channel,
which highlight my IRC client window.

I also had to configure Prometheus alertmanager to avoid some errors:

```nix
services.prometheus = {
  alertmanagers = [{
    scheme = "http";
    path_prefix = "/barracuda-monitoring";
    static_configs = [{
      targets = [
        "localhost:${toString config.services.prometheus.alertmanager.port}"
      ];
    }];
  }];

  alertmanager = {
    enable = true;
    port = 3041;
    configuration = {
      global = { };
      route = {
        receiver = "local-irc";
        group_wait = "30s";
        group_interval = "5m";
        repeat_interval = "4h";
        group_by = [ "alertname" ];

        routes = [
          {
            receiver = "local-irc";
            group_wait = "30s";
            match.severity = "warning";
          }
        ];
      };
      receivers = [
        {
          name = "local-irc";
          webhook_configs = [
            {
              url = "http://127.0.0.1:${toString config.services.prometheus.alertmanagerIrcRelay.settings.http_port}/barracuda-monitoring";
              send_resolved = true;
            }
          ];
        }
      ];
    };
  };

  alertmanagerIrcRelay = {
    enable = true;
    settings = {
      http_host = "localhost";
      http_port = 3040;

      irc_host = "127.0.0.1";
      irc_port = 6502;
      use_privmsg = true;
      irc_use_ssl = false;
      irc_nickname = "alertmanagerIrcRelayBot";

      irc_channels = [
        { name = "#barracuda-monitoring"; }
      ];
    };
  };
};
```

And also Loki, otherwise, you'll get errors in Grafana alert center:

```nix
services.loki = {
  configuration = {
    ruler = {
      enable_api = true;
      enable_alertmanager_v2 = true;
      ring.kvstore.store = "inmemory";
      rule_path = "/var/lib/loki/rules-temp";
      alertmanager_url = "http://127.0.0.1:${toString config.services.prometheus.alertmanager.port}";
      storage = {
        type = "local";
        local.directory = "/var/lib/loki/rules";
      };
    };
  };
};
```

We can finally integrate it in Grafana alert center:

```nix
services.grafana.provision.alerting = {
  contactPoints.settings = {
    contactPoints = [
      {
        name = "local-irc";
        receivers = [{
          uid = "cb3d51ed-e014-4642-9601-426ac954cbed";
          type = "webhook";
          disableResolvedMessage = false;
          settings = {
            url = "http://127.0.0.1:${toString config.services.prometheus.alertmanagerIrcRelay.settings.http_port}/barracuda-monitoring";
          };
        }];
      }
    ];
  };
};
```

Then, we have to set up `local-irc` as default contact point:

```nix
services.grafana.provision.alerting = {
  policies.settings.policies = [{
    receiver = "local-irc";
    group_by = [ "grafana_folder" "alertname" ];
  }];
};
```

We should also drop SMTP default contact point, to avoid confusion:

```nix
services.grafana.provision.alerting = {
  contactPoints.settings = {
    deleteContactPoints = [
      { uid = ""; } # default SMTP
    ];
  };
};
```

and finally add some rules:

```nix
services.grafana.provision.alerting = {
  rules = import ./monitoring/alert-rules.nix {
    inherit pkgs config prometheusGrafanaDataSourceUid lokiGrafanaDataSourceUid;
  };
};
```
