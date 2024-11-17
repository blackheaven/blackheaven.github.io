+++
title = "Homelab: Monitoring alerting through AlertManager"
date = 2023-09-17
draft = false
path = "2023-09/homelab-monitoring-alerting-through-alertmanager"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "monitoring", "lgtm-stack"]
+++

Previously, we have set alerting through [Grafana alerting](@/blog/2023-09-13_homelab-monitoring-alerting.md),
but, if you noticed, I did not expose my alerts more than:

```nix
services.grafana.provision.alerting = {
  rules = import ./monitoring/alert-rules.nix {
    inherit pkgs config prometheusGrafanaDataSourceUid lokiGrafanaDataSourceUid;
  };
};
```

I did it on purpose, as Grafana alerting seems to be mostly designed for GUI/WebUI usage, such as
checking RAM usage take 75 lines in a roughly well formatted layout, and the core of the
alert looks like that:

```nix
{
  refId = "RAM load";
  relativeTimeRange = { from = 3600; to = 0; };
  datasourceUid = prometheusGrafanaDataSourceUid;
  model = {
    datasource = { type = "prometheus"; uid = prometheusGrafanaDataSourceUid; };
    editorMode = "code";
    expr = "clamp_min((1-(node_memory_MemAvailable_bytes/(node_memory_MemTotal_bytes- 4*10^9))), 0)";
    hide = false;
    instant = false;
    interval = "";
    intervalMs = 15000;
    legendFormat = "RAM load";
    maxDataPoints = 43200;
    range = true;
    refId = "RAM load";
  };
}
{
  refId = "Reduce";
  relativeTimeRange = { from = 600; to = 0; };
  datasourceUid = "__expr__";
  model = {
    conditions = [{
      evaluator = { params = [ ]; type = "gt"; };
      operator = { type = "and"; };
      query = { params = [ "A" ]; };
      reducer = { params = [ ]; type = "last"; };
      type = "query";
    }];
    datasource = { type = "__expr__"; uid = "__expr__"; };
    expression = "RAM load";
    hide = false;
    intervalMs = 1000;
    maxDataPoints = 43200;
    reducer = "last";
    refId = "Reduce";
    type = "reduce";
  };
}
{
  refId = "Threshold";
  relativeTimeRange = { from = 600; to = 0; };
  datasourceUid = "__expr__";
  model = {
    conditions = [{
      evaluator = { params = [ 0.5 ]; type = "gt"; };
      operator = { type = "and"; };
      query = { params = [ "B" ]; };
      reducer = { params = [ ]; type = "last"; };
      type = "query";
    }];
    datasource = { type = "__expr__"; uid = "__expr__"; };
    expression = "Reduce";
    hide = false;
    intervalMs = 1000;
    maxDataPoints = 43200;
    refId = "Threshold";
    type = "threshold";
  };
}
```

Not really readable.

On another hand, I came across [Awesome Prometheus Alerts](https://samber.github.io/awesome-prometheus-alerts/)
which is a collection of _alertmanager_ (which is deployed alongside to _Prometheus_).

To highlight the difference, here's the same rule expressed for _alertmanager_:

```nix
{
  alert = "HostOutOfMemory";
  for = "5m";
  expr = ''(node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100 < 80) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
  labels.severity = "warning";
  annotations.summary = "Warning: RAM usage is > 20%";
}
```

7 lines, moreover, the query (in `expr`) is a bit more complex,
because it takes care of multi-node deployment, even though it's not useful.

My whole alerting rules looks like this:

```nix
services.prometheus.rules = map builtins.toJSON [{
  groups = [
    {
      name = "barracuda";
      rules = [
        {
          alert = "HostOutOfMemory";
          for = "5m";
          expr = ''(node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes * 100 < 80) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = "Warning: RAM usage is > 20%";
        }
        {
          alert = "HostOutOfDiskSpaceRoot";
          for = "5m";
          expr = ''((node_filesystem_avail_bytes{mountpoint="/"} * 100) / node_filesystem_size_bytes{mountpoint="/"} < 50 and ON (instance, device, mountpoint) node_filesystem_readonly == 0) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = ''Warning: Root filesystem ("/") is going full (> 50%)'';
        }
        {
          alert = "HostOutOfDiskSpaceBoot";
          for = "5m";
          expr = ''((node_filesystem_avail_bytes{mountpoint="/boot"} * 100) / node_filesystem_size_bytes{mountpoint="/boot"} < 50 and ON (instance, device, mountpoint) node_filesystem_readonly == 0) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = ''Warning: Boot filesystem ("/boot") is going full (> 50%)'';
        }
        {
          alert = "HostHighCpuLoad";
          for = "1m";
          expr = ''(sum by (instance) (avg by (mode, instance) (rate(node_cpu_seconds_total{mode!="idle"}[2m]))) > 0.2) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = "Warning: CPU is unusally overloaded (> 20%)";
        }
        {
          alert = "HostSystemdServiceCrashed";
          for = "0m";
          expr = ''(node_systemd_unit_state{state="failed"} > 0) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = "Warning: systemd service crashed ({{ $value }})";
        }
        {
          alert = "HostNodeOvertemperatureAlarm";
          for = "5m";
          expr = ''((node_hwmon_temp_celsius * ignoring(label) group_left(instance, job, node, sensor) node_hwmon_sensor_label{label!="tctl"} > 50)) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = "Warning: temperature is too high";
        }
      ];
    }
    {
      name = "monitoring";
      rules = [
        {
          alert = "PromtailRequestErrors";
          expr = ''100 * sum(rate(promtail_request_duration_seconds_count{status_code=~"5..|failed"}[1m])) by (namespace, job, route, instance) / sum(rate(promtail_request_duration_seconds_count[1m])) by (namespace, job, route, instance) > 10'';
          for = "5m";
          labels.severity = "critical";
          annotations.summary = "Promtail request errors";
        }
        {
          alert = "PromtailRequestLatency";
          expr = ''histogram_quantile(0.99, sum(rate(promtail_request_duration_seconds_bucket[5m])) by (le)) > 1'';
          for = "5m";
          labels.severity = "critical";
          annotations.summary = "Promtail request latency";
        }
        {
          alert = "LokiProcessTooManyRestarts";
          expr = ''changes(process_start_time_seconds{job=~".*loki.*"}[15m]) > 2'';
          for = "0m";
          labels.severity = "warning";
          annotations.summary = "Loki process too many restarts";
        }
        {
          alert = "LokiRequestErrors";
          expr = ''100 * sum(rate(loki_request_duration_seconds_count{status_code=~"5.."}[1m])) by (namespace, job, route) / sum(rate(loki_request_duration_seconds_count[1m])) by (namespace, job, route) > 10'';
          for = "15m";
          labels.severity = "critical";
          annotations.summary = "Loki request errors";
        }
        {
          alert = "LokiRequestPanic";
          expr = ''sum(increase(loki_panic_total[10m])) by (namespace, job) > 0'';
          for = "5m";
          labels.severity = "critical";
          annotations.summary = "Loki request panic";
        }
        {
          alert = "LokiRequestLatency";
          expr = ''(histogram_quantile(0.99, sum(rate(loki_request_duration_seconds_bucket{route!~"(?i).*tail.*"}[5m])) by (le)))  > 1'';
          for = "5m";
          labels.severity = "critical";
          annotations.summary = "Loki request latency";
        }
      ];
    }
    {
      name = "services";
      interval = "1h";
      rules = [
        {
          alert = "ResticCheckFailed";
          for = "0m";
          expr = ''restic_check_success == 0'';
          labels.severity = "warning";
          annotations.summary = ''Warning: Restic check failed of "{{ $labels.snapshot_paths }}"'';
        }
        {
          alert = "ResticOutdatedBackupLoopingNixosConfig";
          for = "0m";
          expr = ''time() - restic_backup_timestamp{snapshot_paths="/etc/nixos"} > 435600'';
          labels.severity = "critical";
          annotations.summary = ''Critical: Restic last backup of "/etc/nixos" is older than 5 days (and one hour)'';
        }
        {
          alert = "ResticOutdatedBackupLoopingHome";
          for = "0m";
          expr = ''time() - restic_backup_timestamp{snapshot_paths="/home/black"} > 1299600'';
          labels.severity = "critical";
          annotations.summary = ''Critical: Restic last backup of "/home/black" is older than 15 days (and one hour)'';
        }
      ];
    }
    {
      name = "hannibal";
      rules = [
        {
          alert = "HostOutOfDiskSpaceHannibal";
          for = "5m";
          expr = ''((node_filesystem_avail_bytes{mountpoint="/mnt/hannibal_medias"} * 100) / node_filesystem_size_bytes{mountpoint="/mnt/hannibal_medias"} < 20 and ON (instance, device, mountpoint) node_filesystem_readonly == 0) * on(instance) group_left (nodename) node_uname_info{nodename=~".+"}'';
          labels.severity = "warning";
          annotations.summary = ''Warning: Hannibal is going full (> 80%)'';
        }
      ];
    }
  ];
}];
```

Which is 16 rules in 137 lines (roughly 2 Grafana alerts).

Once we have our rules set, we have to redirect them to IRC:

```nix
services.prometheus.alertmanager = {
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
```

Here we are, a fully setup and monitored infrastructure.
