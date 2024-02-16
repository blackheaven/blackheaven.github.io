+++
title = "Bypassing Prometheus SDK push model"
date = 2024-02-07
draft = false
path = "2024-02/bypassing-prometheus"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design", "monitoring", "lgtm-stack", "haskell"]
+++

I have talked [in an earlier log](@/2023-09-24_homelab-monitoring-withings.md)
that I have set up business monitoring which has led to interesting conversations
with the stakeholders.

I have used [prometheus-client](https://hackage.haskell.org/package/prometheus-client)
which is a great start providing `Counter`/`Gauge`/`Summaries`/`Histograms`
but they are push-based, which means: you declare them, and you act on them
(increasing/decreasing/etc.) so they are ready when they are requested.

I have tried to set them on a stateful element (for reference, we receive
requests, which have a step associated to them, there are ~20 different
steps, a typical request going through 30-40 steps, the goal was to have
counter) as a `Gauge` (increasing and decreasing floating point number).

The status changes were event sourced, so I thought: each time a status change
occurs, decrease the previous status' `Gauge` and increase the new status' `Gauge`.

I have tried really hard, but I was ending up with negative `Gauge` and incorrect counts.
More over, I had to maintain a double-accounting piece of code to have coherent counters
(unsuccessfully).

So, I ended up creating a custom `Metric` which does the count once:

```haskell
registerMetrics :: IORef (Map.Map RequestId RequestState) -> IO ()
registerMetrics requestsRef =
  void $ register $ Metric $ return ((), mkSampleGroups)
 where
  mkSampleGroups :: IO [SampleGroup]
  mkSampleGroups = do
    requests <- readIORef requestsRef
    let samples =
          map (\(labels, counts) -> Sample "requests" labels (BS.fromString $ show $ int2Float counts)) $
            Map.toList $
              foldl (flip $ Map.alter (Just . maybe 1 (+ 1))) Map.empty $
                map mkLabels $
                  Map.toList requests
    return
      [ SampleGroup
          (Info "requests" "requests")
          GaugeType
          samples
      ]
```

Which made my `Metric` only relying on my state, and not on a temporal state.
