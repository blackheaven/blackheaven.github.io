+++
title = "Effects systems benchmark"
date = 2025-01-28
draft = false
path = "2025-01/effects-systems-benchmark"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "functional programming"]
+++

Following my [Functional Conf talk](https://confengine.com/conferences/functional-conf-2025),
I have prepared a small [effects systems benchmark](https://github.com/blackheaven/effects-benchmark).

Note: Don't bother watching it, it was painful, I have coughed all the time,
I probably have expelled a piece of lungs at some point (mostly kidding).

For ZuriHac 2020, Alexis King did an amazing work and [talk](https://github.com/lexi-lambda/talks/tree/master/2020-06-effects),
focusing on micro-benchmarking each effects systems, focusing on the raw performances.

It's still a cornerstone to make effects move forward.

However, it became an authority argument in most of the debates I had,
while in production code there's `IO`s and computations.

I've tried to come up with a different one, in which I can inject some `IO`s/computations.

Let's take the basic structure:

```haskell
data Behavior = Behavior { loops :: Int, action :: Int -> IO () }

mkTarget ::
  Monad m =>
  (IO () -> m ()) -> -- lift computation
  m Int -> -- read counter
  (Int -> m ()) -> -- set counter
  Behavior ->
  m ()
mkTarget embed getCounter setCounter Behavior {..} = go
  where go = do
          n <- getCounter
          when (n < loops) $ do
            embed $ action n
            setCounter $ n + 1
            go
```

The firsts arguments are "effect systems"-dependents, dealing with `IO` and state.

For example for `IO`:

```haskell
target ::
    IORef Int ->
    Behavior ->
    IO ()
target ref = mkTarget id (readIORef ref) (writeIORef ref)
```

And for `polysemy`:

```haskell
target ::
    Members '[State Int, Embed IO] r =>
    Behavior ->
    Sem r ()
target = mkTarget embed get put
```

Then we have `Behavior` which is benchmark parameterization, for instance:

```haskell
ioOfMs :: Int -> Int -> IO ()
ioOfMs delay _ = threadDelay $ delay * 1000

pureComputation :: Int -> Int -> IO ()
pureComputation start x = void $ evaluate $ x + read (fib $ show start)

fib :: String -> String
fib =
  \case
    "0" -> "0"
    "1" -> "1"
    n -> show $ read @Int (fib $ show $ read @Int n - 1) + read (fib $ show $ read @Int n - 2)
```

`loops` is always set to `1000`.

Note: for some reason, GHC optimize naive fibonacci sequence aggressively so much,
I suspect it turns it into a `O(n)` function.

Let's have a look at results:

With *no IO/computation*

![](https://github.com/blackheaven/effects-benchmark/raw/master/results/reportNoIOComputation.png)

It's more or less the results shown in the hereinabove mentioned talk.

Let's add *light computation*

![](https://github.com/blackheaven/effects-benchmark/raw/master/results/reportComputationLight.png)

Differences are fading away, ranging from `8.5 ms` to `12.6 ms`.

It's even more compelling with *heavy computation*

![](https://github.com/blackheaven/effects-benchmark/raw/master/results/reportComputationHeavy.png)

Differences are now neglectable, ranging from `67 ms` to `73 ms`.

With an `IO` of `1 ms`

![](https://github.com/blackheaven/effects-benchmark/raw/master/results/reportIO1ms.png)

Differences are already neglectable, ranging from `1.144 s` to `1.157 ms`.

Actually, no API call is that fast, for example,
Redis calls are expected to run in `7 ms`, which gives:

![](https://github.com/blackheaven/effects-benchmark/raw/master/results/reportIO7ms.png)

This benchmark is far from being perfect, for instance, `IO`/computations are run
on each iteration and not each `n`, which could impact the results.

That being said, my hope with this work is to provide a contextualized benchmark,
that way, the pure performance of an effects system would not be the only
decision criteria.
