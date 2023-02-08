+++
title = "Polysemy: Bundle"
date = 2023-02-08
draft = false
path = "2023-02/polysemy-bundle"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Polysemy comes with an useful effect to group other effects: `Bundle`.

It comes with a useful function:

```haskell
sendBundle :: forall e r' r a. (Member e r', Member (Bundle r') r) => Sem (e ': r) a -> Sem r a
```

It can be used as follows:

```haskell
act :: Members '[Bundle '[Trace]] r => Sem r ()
act = do
  sendBundle @Trace @'[Trace] $ trace "Hello"
```

The real benefit associated with `Bundle` are the interpreters.

`runBundle` injects `Bundles`' effects:

```haskell
intrHidden :: Sem '[Bundle '[Trace], Embed IO] a -> IO a
intrHidden = runM . traceToStdout . runBundle
```

While `subsumeBundle` reuse interpreters:

```haskell
intrExplicit :: Sem '[Bundle '[Trace], Trace, Embed IO] a -> IO a
intrExplicit = runM . traceToStdout . subsumeBundle
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Bundle.hs).
