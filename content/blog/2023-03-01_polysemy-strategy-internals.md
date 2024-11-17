+++
title = "Polysemy: Strategy internals"
date = 2023-03-01
draft = false
path = "2023-03/polysemy-strategy-internals"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In previous logs we have seen how to build interpreters thanks to `Strategy`.

```haskell
data BindE (m :: Type -> Type) a where
  BindE :: m a -> (a -> m b) -> BindE m b

makeSem ''BindE

interpretBindFinal :: Member (Final IO) r => InterpreterFor BindE r
interpretBindFinal =
  interpretFinal @IO $
    \case
      BindE f g -> do
        fa <- runS f
        ff <- bindS g
        pure $ fa >>= ff
```

Let's unbox `interpretFinal`:

```haskell
interpretFinal
    :: forall m e r a.
    Member (Final m) r
    => (forall x rInitial. e (Sem rInitial) x -> Strategic m (Sem rInitial) x)
    -> Sem (e ': r) a
    -> Sem r a
interpretFinal n =
  let
    go :: Sem (e ': r) x -> Sem r x
    go = hoistSem $ \u -> case decomp u of
      Right (Weaving e s wv ex ins) ->
        injWeaving $
          Weaving
            (WithWeavingToFinal (runStrategy (n e)))
            s
            (go . wv)
            ex
            ins
      Left g -> hoist go g
  in
    go
```

To give a brief overview:
* `Union m r a` is one of the parts of `Sem` and represents effects
* `injWeaving` adds an effect to this `Union`
* `decomp` pops out the first effect in the `Union`, selecting the right one (thanks to types)
* `hoist` and `hoistSem` change the lifted Monads
* `runStrategy` is part of `Strategy`
* `WithWeavingToFinal` is part of `Final`

Strategy is straightforward:

```haskell
data Strategy m f n z a where
  GetInitialState     :: Strategy m f n z (f ())
  HoistInterpretation :: (a -> n b) -> Strategy m f n z (f a -> m (f b))
  GetInspector        :: Strategy m f n z (Inspector f)

type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (m (f a))

type WithStrategy m f n = '[Strategy m f n]

runStrategy :: Functor f
            => Sem '[Strategy m f n] a
            -> f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> a
runStrategy sem = \s wv ins -> run $ interpret
  (\case
    GetInitialState       -> pure s
    HoistInterpretation f -> pure $ \fa -> wv (f <$> fa)
    GetInspector          -> pure (Inspector ins)
  ) sem
```

to sum it up: `Strategy` is mostly a way to inject functions, it is used as follows in `Final`:

```haskell
newtype Final m z a where
  WithWeavingToFinal
    :: ThroughWeavingToFinal m z a
    -> Final m z a

type ThroughWeavingToFinal m z a =
     forall f
   . Functor f
  => f ()
  -> (forall x. f (z x) -> m (f x))
  -> (forall x. f x -> Maybe x)
  -> m (f a)
```

As you see: `ThroughWeavingToFinal` has the type of `runStrategy`, which makes `Final` and `Strategy` intertwined, and highlight the relevance compared to `Embed`.

