+++
title = "Polysemy: Interpretation and effects inline injection"
date = 2022-12-18
draft = false
path = "2022-12/polysemy-interpretation-effects-inline-injection"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In a [previous log](./2022-12-14_polysemy-interpretation-effects-injection.md) we have seen a way to cache computation and a way to inject effects.

Let see what it take to build a caching effect:

```haskell
data Cache k v (m :: Type -> Type) a where
  Cached :: k -> Cache k v m v
```

As opposed to `View`, we have a cache per key and not a global cache, it could lead to memory leaks.

We can use it as follows:

```haskell
app :: Members '[Embed IO, Cache String String] r => Sem r ()
app = do
  name0 <- cached "name"
  embed $ putStrLn $ "Your name is " <> name0
  location0 <- cached "location"
  embed $ putStrLn $ "Your location is " <> location0
  name1 <- cached "name"
  location1 <- cached "location"
  embed $ putStrLn $ "Hello " <> name1 <> " from " <> location1
```

It let use with the interpreter:

```haskell
runCache :: forall k v r. Ord k => (k -> Sem r v) -> InterpreterFor (Cache k v) r
runCache f =
  evalState mempty
  . interpret (\case
        Cached k -> do
          currentCache <- get @(M.Map k v)
          case currentCache M.!? k of
            Nothing -> do
              v <- raise $ f k
              put $ M.insert k v currentCache
              return v
            Just v -> return v
      )
  . raiseUnder @(State (M.Map k v))
```

It can be understood this way:

1. Add a new effect with `raiseUnder` (so we have `(State (M.Map k v) ': r)`)
2. Get current cache
3. Run the computation when key is missing (here we are forced to use `raise` since `f` only gives `Sem r`)
4. Run the `State` effect

Finally, here's what it gives:

```
What's your name?
Gautier
Your name is Gautier
What's your location?
France
Your location is France
Hello Gautier from France
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/EffectInlineInjection.hs).
