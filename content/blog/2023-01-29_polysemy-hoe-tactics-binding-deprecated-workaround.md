+++
title = "Polysemy: Tactics binding deprecated workaround"
date = 2023-01-29
draft = false
path = "2023-01/polysemy-tactics-binding-deprecated-workaround"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/blog/2023-01-25_polysemy-hoe-tactics-binding.md), we ended with a solution for `Resource` which wasn't `Error` resistant.

The solution is to use the soo-deperecated [`withLowerToIO`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy-Internal-Forklift.html).

Let's start again with our simpler effect

```haskell
data BindE (m :: Type -> Type) a where
  BindE :: m a -> (a -> m b) -> BindE m b

makeSem ''BindE
```

We can express an impure (`Embed`-base) interpreter:

```haskell
interpretBindTacticLowering :: forall r. Member (Embed IO) r => InterpreterFor BindE r
interpretBindTacticLowering =
  interpretH $
    \case
      BindE f g -> do
        ma <- runT f
        mf <- bindT g
        withLowerToIO $ \lower _ -> do
          let toIO :: Sem (BindE ': r) x -> IO x
              toIO = lower . raise . interpretBindTacticLowering
          toIO ma >>= toIO . mf
```

`withLowerToIO` provides two functions (one to locally interpret a `Sem r a` to `IO a` and a finalizer) and expect a `IO a`, it's defined as:

```haskell
withLowerToIO
    :: Member (Embed IO) r
    => ((forall x. Sem r x -> IO x) -> IO () -> IO a)
    -> Sem r a
```

Under the hood, a thread is created (the program should be compiled with `-thread`) to run the effect stack until `IO`, 'blocking' thread.

The finalizer does not need to be called, but it indicate the end of the new thread.

Let's see how it goes for `Resource`:

```haskell
data Resource m a where
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Resource m b
```

We are now able to use `bracket`:

```haskell
resourceToIO :: Member (Embed IO) r => InterpreterFor Resource r
resourceToIO =
  interpretH $
    \case
      Bracket alloc dealloc use -> do
        alloc' <- runS alloc
        dealloc' <- bindS dealloc
        use' <- bindS use

        withLowerToIO $ \lower finish -> do
          let runHoE :: Sem (Resource ': r) x -> IO x
              runHoE = lower . raise . resourceToIO
          Expection.bracket
              (runHoE alloc')
              (\x -> runHoE (use' x) >> finish)
              (runHoE . dealloc')
```

See the full the code [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Resource.html) and [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Bind.hs).
