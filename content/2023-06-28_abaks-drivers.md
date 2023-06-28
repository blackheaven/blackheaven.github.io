+++
title = "Abaks: Drivers"
date = 2023-06-28
draft = false
path = "2023-06/abaks-drivers"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

Last time we introduced [Interface Adapters](@/2023-06-25_abaks-interface-adapters-api.md) with [_servant_](https://docs.servant.dev/en/stable/) to serve a HTTP (REST) API.

But we've been left with some `Sem r`-based definition we have to transform to fit in _servant_ types:

Let's start by setting the layout:

```haskell
main :: IO ()
main = do
  let settings = setPort 8080 $ setHost "0.0.0.0" defaultSettings
  eventSourcingSettings <- EventSourcing.mkFileSettings "abaks-aggregates.json"
  runSettings settings $
    serve (Proxy @API) $
      hoistServer (Proxy @API) (hoistHandler eventSourcingSettings) server
```

The key here is `hoistServer` which takes a function to convert `Sem r a` to _servant_'s `Handler a`.

And, surprise surprise, `hoistServer` is a classical interpreter:

```haskell
hoistHandler ::
  EventSourcing.FileSettings ->
  Sem (Append ApiEffects '[Final IO]) a ->
  Handler a
hoistHandler eventSourcingSettings =
  Handler
    . ExceptT
    . runFinal
    . runError
    . Random.runRandom
    . EventSourcing.runFile eventSourcingSettings
```
