+++
title = "Homelab: Monitoring Withings"
date = 2023-09-24
draft = false
path = "2023-09/homelab-monitoring-withings"

[taxonomies]
categories = ["ops"]
tags = ["ops", "development", "monitoring", "lgtm-stack"]
+++

I have a simple morning routing starting with:

- Getting up
- Opening the shutters
- Making my bed
- Petting my cat (eventually, opening the door to let her out)
- Emptying my bladder
- Getting on my Withings smart scale

Each Sunday, I take additional measurements, and I log them and my week's 
average week weight in a multiple-years LibreOffice Calc (don't judge...).

For the record, I was using Fitbit until [Google acquired it](https://blog.google/products/devices-services/fitbit-acquisition/)
(and degraded it: [1](https://www.makeuseof.com/ways-google-ruined-fitbit/), [2](https://www.techradar.com/opinion/fitbit-as-we-know-it-is-already-dead-thanks-to-google), [3](https://www.gizchina.com/2023/03/02/fitbit-is-dead-google-killed-it/), etc.).

The thing is, there's no way to get week average weight, so, after few months
of manually making averages, I dirtily pushed [withings-weights](https://github.com/blackheaven/withings-weights).

To illustrate my journey through [monitoring](content/2023-09-06_homelab-monitoring-introduction.md)
(especially [alerting](@/blog/2023-09-13_homelab-monitoring-alerting.md)), I would like to
add some metrics and alert (as I would do for business systems).

I have found [prometheus-client](https://hackage.haskell.org/package/prometheus-client) on hackage
which is a great fit (and [prometheus-metrics-ghc](https://hackage.haskell.org/package/prometheus-metrics-ghc), [servant-prometheus](https://hackage.haskell.org/package/servant-prometheus), [wai-middleware-prometheus](https://hackage.haskell.org/package/wai-middleware-prometheus)).

Let's start with generic code:

```haskell
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC
import Prometheus.Servant

main :: IO ()
main = do
  -- ...
  register ghcMetrics

  let servantPMW = prometheusMiddleware defaultMetrics $ Proxy @API
  run serverEnv.serverPort $ prometheus def $ servantPMW $ app serverEnv oauthEnv info
```

We can highlight three parts in the above snippet:

* `register ghcMetrics` add the GHC metrics in the global metric registry
* `prometheusMiddleware` is a middleware which collect metrics around `servant` endpoints
* `prometheus` is a middleware which add further instrumentation around `wai` and a `/metrics` endpoint to display collected metrics

Then we can define few metrics:

```haskell
data WithingsMetrics = WithingsMetrics
  { lastChecked :: Vector Text Gauge,
    lastWeight :: Vector Text Gauge,
    users :: Counter
  }
```

There are few kind of metrics:

* `Counter`: always increasing values
* `Gauge`: variable values
* `Summary`/`Histogram`: observations made over time

all of them being floating point numbers (`Double`).

`Vector a m` are partitioned metrics (`m`) or labels (`a`).

Let's instantiate them:

```haskell
main :: IO ()
main = do
  -- ...
  register ghcMetrics
  metrics <-
    WithingsMetrics
      <$> register (vector "username" $ gauge (Info "withings_last_checked" "Last time a User checked his/her stats"))
      <*> register (vector "username" $ gauge (Info "withings_last_weight" "Last User weight"))
      <*> register (counter (Info "withings_users" "Users count"))

  addCounter metrics.users . fromIntegral . length =<< runHandler (listUsers info)

  let servantPMW = prometheusMiddleware defaultMetrics $ Proxy @API
  run serverEnv.serverPort $ prometheus def $ servantPMW $ app serverEnv oauthEnv info metrics
```

We can notice:

* `Info` declares metric name and description
* `vector` declares labels

Finally, we used `addCounter` to pre-populate user count at startup.

We can continue with endpoint instrumentation:

```haskell
retrieveOauthHandler :: OauthEnv -> UsersInfo -> WithingsMetrics -> Text -> Text -> Handler Text
retrieveOauthHandler env info metrics code state = do
  user <- fetchOauthTokens env info code state
  liftIO $ incCounter metrics.users
  -- ...

statsHandler :: OauthEnv -> UsersInfo -> WithingsMetrics -> UserName -> Handler Text
statsHandler env info metrics user = do
  groupedWeights <- fetchStats $ withOauthBearer env info user
  let fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
      fromFixed fv@(MkFixed v) = (fromIntegral v) / (fromIntegral $ resolution fv)
  nowGaugeValue <- fromFixed . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  liftIO $
    withLabel metrics.lastChecked user.getUserName $ \metric ->
      setGauge metric nowGaugeValue
  -- ...
```

So far so good, if we `curl` the `/metrics` endpoint:

```
# HELP withings_users Users count
# TYPE withings_users counter
withings_users 1.0
# HELP withings_last_checked Last time a User checked his/her stats
# TYPE withings_last_checked gauge
withings_last_checked{username="Gautier"} 1.6955709670325263e9
# HELP withings_last_weight Last User weight
# TYPE withings_last_weight gauge
withings_last_weight{username="Gautier"} 67.34
```

Finally we  can set up some rules:

```nix
{
  alert = "WeightLogForgotten";
  for = "0m";
  expr = ''time() - withings_last_checked{username="Gautier"} > 691200'';
  labels.severity = "info";
  annotations.summary = ''Info: logging weights should be done (last time was > 8 days ago)'';
}
{
  alert = "WeightUnder";
  for = "0m";
  expr = ''withings_last_weight{username="Gautier"} < 66.5'';
  labels.severity = "warning";
  annotations.summary = ''Warning: you have lost too much weight (< 66.5 kg), call your nutritionist'';
}
{
  alert = "WeightSignup";
  for = "0m";
  expr = ''withings_users > 1'';
  labels.severity = "critical";
  annotations.summary = ''Critical: someone signed up, you have been breached'';
}
```

So far so good, on a real project, I would create an associated dashboard and
(probably) share it with stakeholders.
