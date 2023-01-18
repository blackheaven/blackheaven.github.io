+++
title = "Polysemy: Async"
date = 2023-01-18
draft = false
path = "2023-01/polysemy-async"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Polysemy comes with many effects, today we'll have a look at [`Async`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy-Async.html) which is the effect for and over [`async`](https://hackage.haskell.org/package/async) package.

It enables running and driving asynchronous operations without leaving the effect monad.

Let's review the effect:

```haskell
data Async m a where
  Async :: m a -> Async m (Async.Async (Maybe a))
  Await :: Async.Async a -> Async m a
  Cancel :: Async.Async a -> Async m ()
```

Actually, unlike `async` package, `async` operation produces an `Async (Maybe a)`, let's see one interpreter to understand why:

```haskell
asyncToIOFinal :: Member (Final IO) r => InterpreterFor Async r
asyncToIOFinal =
  interpretFinal @IO $
    \case
      Async act -> do
        act'  <- runS act
        inspector <- getInspectorS
        liftS $ Async.async (inspect inspector <$> act')
      Await x -> liftS $ Async.wait x
      Cancel x -> liftS $ Async.cancel x
```

There is a lot of things going on, let's see what's going on in `Async` case.

First we convert the given Monad (`act`) to the local Monad, which is `Sem (WithStrategy m f n)` (we'll dig deeper in another log).
`act'` has the type `m (f a)`.

Then we get the `Inspector`, which is defined as follows:

```haskell
newtype Inspector f = Inspector
  { inspect :: forall x. f x -> Maybe x
  }
```

It is used to attempt to fetch a value from the effect monad.

```haskell
inspector <- getInspectorS
fa <- pureS "hello"
fb <- pureS True
let aM = inspect inspector fa   -- Just "hello"
    bM = inspect inspector fb   -- Just True
```

Whenever you use a `pureS`, `runS`, etc. a `f a` typed-value is created.

This `f` is a context builtin the high-order effect interpretation, the only way to extract the value from it is to use the `Inspector`.

The `Maybe` is present since you can't guarantee that an `Error` (or any workflow-changing effect) didn't happen, breaking type checking.

That's the reason why `async` produce a `Async (Maybe a)`.

As you can see in the last line, we run the inspection on `act'` (to get a `IO (Maybe a)`).
Run `async`'s `async` and `liftS` the resulting `IO (Async (Maybe a))`.

See the full the code [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Async.html).
