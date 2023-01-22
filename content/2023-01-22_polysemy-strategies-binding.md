+++
title = "Polysemy: Strategies binding"
date = 2023-01-22
draft = false
path = "2023-01/polysemy-strategies-binding"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/2023-01-18_polysemy-async.md), we introduced `liftS` and `runS` which helps running higher-order effects in the interpretation Monad.

However, you sometime have effects with multiple higher-order effects needing to be bound, sur as [`Resource`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy-Resource.html):

```haskell
data Resource m a where
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Resource m b
```

In order to have a working interpreter we have to use `bindS`:

```haskell
resourceToIOFinal :: Member (Final IO) r => InterpreterFor Resource r
resourceToIOFinal =
  interpretFinal @IO $
    \case
      Bracket alloc dealloc use -> do
        alloc' <- runS alloc
        dealloc' <- bindS dealloc
        use' <- bindS use
        return $ Exception.bracket alloc' dealloc' use'
```

`bindS` is pretty straightforward:

```haskell
bindS :: (a -> n b) -> Sem (WithStrategy m f n) (f a -> m (f b))
```

With a simpler effect:

```haskell
data BindE (m :: Type -> Type) a where
  BindE :: m a -> (a -> m b) -> BindE m b

makeSem ''BindE
```

We see that a simple bind does the trick:

```haskell
interpretBindFinal :: Member (Final IO) r => InterpreterFor BindE r
interpretBindFinal =
  interpretFinal @IO $
    \case
      BindE f g -> do
        fa <- runS f
        ff <- bindS g
        pure $ fa >>= ff
```

See the full the code [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Resource.html) and [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Bind.hs).
