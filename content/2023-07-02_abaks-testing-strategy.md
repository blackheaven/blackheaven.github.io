+++
title = "Abaks: A word on testing"
date = 2023-07-02
draft = false
path = "2023-07/abaks-testing-strategy"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

If you followed this series, you may have noticed that, neither in [Interface Adapters](@/2023-06-25_abaks-interface-adapters-api.md) nor in [Drivers](@/2023-06-28_abaks-drivers.md).

For _Interface Adapters_, I don't test it because, on one hand, it's quite painful:

```haskell
it "All chained commands should work" $ \Fixture {..} -> do
  let createPeriodRequest =
        CreatePeriodRequest
          { name = "Jun 2023",
            initialBalance = AmountA 2000,
            from = fromGregorian 2023 6 1,
            to = fromGregorian 2023 6 30
          }
  CreatePeriodResponse periodId <- successful $ apiCall $ apiClient // API.createPeriodAPI /: createPeriodRequest
  void $ successful $ apiCall $ apiClient // API.fetchPeriodAPI /: periodId
```

on another hand it's mostly wrapping code (which are tested independently) which gives a ratio pain/bug catched very high.
Note: In the past, I tend to put access control (missing in abaks) in _Interface Adapters_, but I tend to put it in _Use Cases_, it doesn't change much in the end (since I design access control as an eDSL), but it helps to stabilize roles' access rights.

For _Drivers_, it's only wiring, I could spend time setting-up a complete environment (spawning containers, setting environment variables), but it would pointless.
I rely on deployment tests to ensure everything is wired correctly (even though the only mistake you can is to pick the wrong interpreter).
