+++
title = "Homelab: Monitoring introduction"
date = 2023-09-06
draft = false
path = "2023-09/homelab-monitoring-introduction"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "monitoring", "lgtm-stack"]
+++

As you might have guest in [my previous log](@/2023-08-02_dependencies-management.md),
I'm obsessed with outages.

There are two ways to detect and outage:

* Have patient users
* Have a monitoring system

Let's be honest, I'm not patient.

In France, we have a saying:

> Charité bien ordonné commence par soi-même
> 
> _(Charity begins at home)_

Let's start with the [LGTM stack](https://grafana.com/oss/).

Hopefully NixOS supports it well.

We can start with Prometheus (which exposes endpoints with metrics):

```nix
services.prometheus = {
  port = 3020;
  enable = true;

  exporters = {
    node = {
      port = 3021;
      enabledCollectors = [ "systemd" ];
      enable = true;
    };
  };

  scrapeConfigs = [{
    job_name = "nodes";
    static_configs = [{
      targets = [
        "127.0.0.1:${toString config.services.prometheus.exporters.node.port}"
      ];
    }];
  }];
};
```

Then Loki, which receives logs and exposes an endpoint with logs:

```nix
services.loki = {
  enable = true;
  configuration = {
    server.http_listen_port = 3030;
    auth_enabled = false;

    ingester = {
      lifecycler = {
        address = "127.0.0.1";
        ring = {
          kvstore = {
            store = "inmemory";
          };
          replication_factor = 1;
        };
      };
      chunk_idle_period = "1h";
      max_chunk_age = "1h";
      chunk_target_size = 999999;
      chunk_retain_period = "30s";
      max_transfer_retries = 0;
    };

    schema_config = {
      configs = [{
        from = "2022-06-06";
        store = "boltdb-shipper";
        object_store = "filesystem";
        schema = "v11";
        index = {
          prefix = "index_";
          period = "24h";
        };
      }];
    };

    storage_config = {
      boltdb_shipper = {
        active_index_directory = "/var/lib/loki/boltdb-shipper-active";
        cache_location = "/var/lib/loki/boltdb-shipper-cache";
        cache_ttl = "24h";
        shared_store = "filesystem";
      };

      filesystem = {
        directory = "/var/lib/loki/chunks";
      };
    };

    limits_config = {
      reject_old_samples = true;
      reject_old_samples_max_age = "168h";
    };

    chunk_store_config = {
      max_look_back_period = "0s";
    };

    table_manager = {
      retention_deletes_enabled = false;
      retention_period = "0s";
    };

    compactor = {
      working_directory = "/var/lib/loki";
      shared_store = "filesystem";
      compactor_ring = {
        kvstore = {
          store = "inmemory";
        };
      };
    };
  };
};
```

And Promtail to push logs to Loki:

```nix
services.promtail = {
  enable = true;
  configuration = {
    server = {
      http_listen_port = 3031;
      grpc_listen_port = 0;
    };
    positions = {
      filename = "/tmp/positions.yaml";
    };
    clients = [{
      url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}/loki/api/v1/push";
    }];
    scrape_configs = [{
      job_name = "journal";
      journal = {
        max_age = "31d";
        labels = {
          job = "systemd-journal";
          host = "barracuda";
        };
      };
      relabel_configs = [{
        source_labels = [ "__journal__systemd_unit" ];
        target_label = "unit";
      }];
    }];
  };
};
```

Finally Grafana to have an UI:

```nix
services.grafana = {
  enable = true;

  settings = {
    analytics.reporting_enable = false;
    server = {
      root_url = "https://monitoring.barracuda.local";
      http_port = 3010;
      http_addr = "127.0.0.1";
      protocol = "http";
    };
    security = {
      admin_user = "admin";
      admin_password = "admin";
    };
  };

  provision = {
    enable = true;
    datasources.settings.datasources = [
      {
        uid = "cd47d5c1-f607-418e-a51c-080cbf31fefb";
        name = "Local Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "http://127.0.0.1:${toString config.services.prometheus.port}";
      }
      {
        uid = "3d92e8be-db1e-4808-ac93-c96c61f01fe0";
        name = "Local Loki";
        type = "loki";
        access = "proxy";
        url = "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}";
      }
    ];
  };
};
```

Note that, I have setup random UUIDs (well, ViM did) to make easier
to provision dashboards and alerting (in follow-ups).
