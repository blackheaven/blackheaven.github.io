+++
title = "Polysemy: AtomicState"
date = 2023-02-15
draft = false
path = "2023-02/polysemy-atomicstate"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

One of the effect used over and over again in our previous example is `State`, however, it's lack of atomicity can cause problems:

```haskell
incHundred :: Members '[State Int] r => Sem r ()
incHundred =
  forM_ ([0 .. 99] :: [Int]) $ \_ ->
    modify' @Int (+ 1)

incThousand :: Members '[State Int, Async] r => Sem r ()
incThousand =
  void $ sequenceConcurrently $ map (const incHundred) ([0 .. 9] :: [Int])
```

You might not get a `1000` here.

That why `AtomicState` is useful:

```haskell
data AtomicState s m a where
  AtomicState :: (s -> (s, a)) -> AtomicState s m a
  AtomicGet   :: AtomicState s m s
```

We can rewrite your code:

```haskell
incHundred' :: Members '[AtomicState Int] r => Sem r ()
incHundred' =
  forM_ ([0 .. 99] :: [Int]) $ \_ ->
    atomicModify' @Int (+ 1)

incThousand' :: Members '[AtomicState Int, Async] r => Sem r ()
incThousand' =
  void $ sequenceConcurrently $ map (const incHundred') ([0 .. 9] :: [Int])
```

Not a big change.

Basically, `AtomicState` provides interpreters that rely on atomic operations (`runAtomicStateIORef` and `atomicStateToIO` over `IORef` and `runAtomicStateTVar` over `TVar`).

However, beware that other interpreters (such as `atomicStateToState`) rely on `State` and break the behavior.

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/AtomicState.hs).
