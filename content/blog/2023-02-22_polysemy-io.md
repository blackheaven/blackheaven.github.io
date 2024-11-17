+++
title = "Polysemy: IO"
date = 2023-02-22
draft = false
path = "2023-02/polysemy-io"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Polysemy provides many ways to combine both effects and interpreters, let's start with a simple `Db` effects:

```haskell
data Db (m :: Type -> Type) a where
  ExecQuery :: Statement args a -> args -> Db m a
```

Some libraries provide dedicated Monad which are expensive to run, for example:

```haskell
runQuery :: Statement args a -> args -> MonadDb a
```

Would force us to embed it:

```haskell
runDb :: forall r. Member (Embed MonadDb) r => InterpreterFor Db r
runDb =
  interpret $
    \case
      ExecQuery s args -> embed $ runQuery s args
```

We have many interpretations options:

```haskell
embedToMonadIO :: forall m r a. (MonadIO m, Member (Embed m) r) => Sem (Embed IO ': r) a -> Sem r a
```

If `MonadDb` is a `MonadIO` instance, we can use it as such:

```haskell
runMonadDb $ runM $ embedToMonadIO @MonadDb $ runDb act
```

Everything will be run as `MonadDb`.

Under the hood, it uses `runEmbedded`:

```haskell
runEmbedded
  :: forall m1 m2 r a.
  Member (Embed m2) r
  => (forall x. m1 x -> m2 x)
  -> InterpreterFor (Embed m1) r
```

Which is more straightforward to use:

```haskell
  runM $ runEmbedded runMonadDb $ runDb act
```

A last option is to use `lowerEmbedded`.

```haskell
lowerEmbedded
  :: (MonadIO m, Member (Embed IO) r)
  => (forall x. m x -> IO x)
  -> InterpreterFor (Embed m) r
```

It creates a run thread to deal with the lowering.

```haskell
runM $ lowerEmbedded runMonadDb $ runDb act
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/IO.hs).
