+++
title = "Polysemy: Introduction to interpreters"
date = 2022-11-27
draft = false
path = "2022-11/polysemy-interpreters-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

As seen in the [previous log](@/2022-11-23_polysemy-intro.md) we have an expression as follows:

```haskell
import Polysemy
import Polysemy.Trace

displayFile :: FilePath -> Sem '[Trace, Embed IO] Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content
```

In order to do something useful we need to interpret them in term of `IO`:

```haskell
intrDisplayFile :: Sem '[Trace, Embed IO] a -> IO a
intrDisplayFile =
  runM
    . traceToStdout
```

It can be read that way:

* `traceToStdout` interprets `Trace` (after the application we have `Sem '[Embed IO] a`)
* `runM` lowers the `Embed IO`

One of the benefit of _effects systems_ is to be able to change the behavior (via the interpretation) without changing the expression.

As such `Trace` could be:

* Redirected to `stderr` with `traceToStderr`
* Redirected to a `Handle` with `traceToHandle`
* Dropped with `ignoreTrace`
* Accumulated with `runTraceList`
* etc.

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Intro.hs).
