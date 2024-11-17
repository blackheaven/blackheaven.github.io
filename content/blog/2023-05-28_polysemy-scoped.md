+++
title = "Polysemy: Scoped"
date = 2023-05-28
draft = false
path = "2023-05/polysemy-scoped"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

With [Polysemy 1.9.0.0](@/blog/2023-05-21_polysemy-v19.md) came the `Scoped` effect.

It's defined as follows:

```haskell
data Scoped (param :: Type) (effect :: Effect) :: Effect where
  Run :: forall param effect m a. Word -> effect m a -> Scoped param effect m a
  InScope :: forall param effect m a. param -> (Word -> m a) -> Scoped param effect m a
```

Complex, the interesting function is `scoped`:

```haskell
scoped ::
    forall param effect r.
    Member (Scoped param effect) r =>
    param ->
    InterpreterFor effect r
```

Lets imagine a simple `Queue` effect:

```haskell
data Queue q (m :: Type -> Type) a where
  Dequeue :: Queue q m (Maybe q)

makeSem ''Queue
```

we can use it as follows:

```haskell
works :: Members '[Scoped (Maybe ()) (Queue ()), Error (), Embed IO] r => Sem r ()
works =
  scoped @_ @(Queue ()) (Just ()) $ do
    void $ dequeue @()
    void $ dequeue @()

fails :: Members '[Scoped (Maybe ()) (Queue ()), Error (), Embed IO] r => Sem r ()
fails =
  scoped @_ @(Queue ()) (Just ()) $ do
    void $ dequeue @()
    () <- throw ()
    void $ dequeue @()
```

We define a 1-element `Queue`.

We can then produce an interpreter allocating a `MVar`

```haskell
withQueue ::
  Members '[Embed IO] r =>
  Maybe q ->
  (MVar q -> Sem r a) ->
  Sem r a
withQueue initial use = do
  mvar <- embed $ maybe newEmptyMVar newMVar initial
  use mvar

interpretQueue ::
  forall q r.
  Members '[Embed IO] r =>
  InterpreterFor (Scoped (Maybe q) (Queue q)) r
interpretQueue =
  interpretScopedWith @'[] withQueue $ \mvar -> \case
    Dequeue -> do
      embed $ putStrLn "deqeue"
      embed $ tryTakeMVar mvar
```

Usable as follows:

```haskell
putStrLn "Work"
runM (runError @() $ interpretQueue @() works) >>= print
putStrLn "Fail"
runM (runError @() $ interpretQueue @() fails) >>= print
```

Giving:

```
Work
deqeue
deqeue
Right ()
Fail
deqeue
Left ()
```

You can notice `@[]` in:

```haskell
  interpretScopedWith @'[] withQueue $ \mvar -> \case
```

it's actually a way to inject effects during interpretation:

```haskell
withQueue' ::
  Members '[Embed IO] r =>
  Maybe q ->
  (() -> Sem (Stack q : r) a) ->
  Sem r a
withQueue' initial use = do
  mvar <- embed $ maybe newEmptyMVar newMVar initial
  runStackMVar mvar $ use ()

runStackMVar :: Members '[Embed IO] r => MVar q -> InterpreterFor (Stack q) r
runStackMVar mvar =
  interpret $
    \case
      Pop -> embed $ tryTakeMVar mvar

interpretQueue' ::
  forall q r.
  Members '[Embed IO] r =>
  InterpreterFor (Scoped (Maybe q) (Queue q)) r
interpretQueue' =
  interpretScopedWith @'[Stack q] withQueue' $ \() -> \case
    Dequeue -> do
      embed $ putStrLn "deqeue"
      pop
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy19/src/Scoped.hs).

