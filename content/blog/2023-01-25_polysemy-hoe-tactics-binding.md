+++
title = "Polysemy: Tactics binding"
date = 2023-01-25
draft = false
path = "2023-01/polysemy-tactics-binding"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/blog/2023-01-22_polysemy-strategies-binding.md), we introduced `bindS` providing a way to bind higher-order effects.

Tactics also provide `*T` equivalent, starting with our example:

```haskell
data BindE (m :: Type -> Type) a where
  BindE :: m a -> (a -> m b) -> BindE m b

makeSem ''BindE
```

We can express a 'pure' interpreter:

```haskell
interpretBindTactic :: InterpreterFor BindE r
interpretBindTactic =
  interpretH $
    \case
      BindE f g -> do
        ma <- runT f
        mf <- bindT g
        let runHoE = raise . interpretBindTactic
        runHoE ma >>= runHoE . mf
```

Unlike its Strategic equivalent, we are forced to explicitly interpret higher-order effects (`runHoE` helper).

We'll see what we can do with this extra freedom.

Let's see how it goes for `Resource`:

```haskell
data Resource m a where
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Resource m b
```

In order to have a working interpreter we have to use `bindT`:

```haskell
runResource :: InterpreterFor Resource r
runResource =
  interpretH $
    \case
      Bracket alloc dealloc use -> do
        alloc' <- runS alloc
        dealloc' <- bindS dealloc
        use' <- bindS use

        let runHoE = raise . runResource
        resource <- runHoE alloc'
        result <- runHoE $ use' resource
        _ <- runHoE $ dealloc' resource
        pure result
```

Nothing new here, however, we are not relying on `bracket` anymore, so, if an error occurs, `dealloc` might not be run.

See the full the code [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Resource.html) and [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Bind.hs).
