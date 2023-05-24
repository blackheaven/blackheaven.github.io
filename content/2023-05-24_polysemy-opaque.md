+++
title = "Polysemy: Opaque"
date = 2023-05-24
draft = false
path = "2023-05/polysemy-opaque"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

With [Polysemy 1.9.0.0](@/2023-05-21_polysemy-v19.md) came the `Opaque` effect.

It's defined as follows:

```haskell
newtype Opaque (e :: Effect) m a where
  Opaque :: forall e m a. e m a -> Opaque e m a
```

If you recall, it looks like [`Tagged`](@/2022-12-21_polysemy-tagged-effects.md)

```haskell
newtype Tagged k e m a where
  Tagged :: forall k e m a. e m a -> Tagged k e m a
```

It just lacks the tag name (a phantom type).

It comes with two functions:

```haskell
toOpaque :: Sem (e ': r) a -> Sem (Opaque e ': r) a
fromOpaque :: Sem (Opaque e ': r) a -> Sem (e ': r) a
```

It's particularly useful when you have type variables as effects:

```haskell
wrong :: Sem (e ': Trace ': r) ()
wrong = trace "Wrong"
```

will produce:

```haskell
    • Overlapping instances for Member Trace (e : Trace : r)
        arising from a use of ‘trace’
      Matching instances:
        two instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
      (The choice depends on the instantiation of ‘e, r’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the expression: trace "Wrong"
      In an equation for ‘wrong’: wrong = trace "Wrong"
  |
  | wrong = trace "Wrong"
  |
```

While, this works:

```haskell
ok :: Sem (e ': Trace ': r) ()
ok = fromOpaque $ trace "Works"
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy19/src/Opaque.hs).

