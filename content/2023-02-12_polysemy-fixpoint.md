+++
title = "Polysemy: Fixpoint"
date = 2023-02-12
draft = false
path = "2023-02/polysemy-fixpoint"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

As part of the Haskell folklore, Polysemy supports `MonadFix` for with `Fixpoint`.

Let's imagine a usecase where we want to find the sources of a source:

```haskell
type Source = String

type Sources = Map.Map Source Source

sources :: Sources
sources =
  Map.fromList
    [ ("4", "3"),
      ("3", "2"),
      ("2", "1"),
      ("1", "0")
    ]

fetchSource :: Members '[Reader Sources, Final IO] r => Source -> Sem r (Maybe Source)
fetchSource s = do
  embedFinal $ putStrLn $ "Asked: " <> show s
  Map.lookup s <$> ask
```

Adding `Fixpoint` allows `(m)do` statements reordering:

```haskell
findRoute4Path :: Members '[Reader Sources, Fixpoint, Final IO] r => Source -> Sem r Source
findRoute4Path i = mdo
  let fetch x = fromMaybe i <$> fetchSource x
      path = init $ concatMap (<> ".") [s0, s1, s2, s3]
  s0 <- fetch i
  s1 <- fetch s0
  s2 <- fetch s1
  s3 <- fetch s2
  return path
```

Giving:

```
Asked: "4"
Asked: "3"
Asked: "2"
Asked: "1"
"3.2.1.0"
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Fixpoint.hs).

PS: Actually, a more useful application can be acheive with `fix`:

```haskell
findRoot :: Members '[Reader Sources, Fixpoint, Final IO] r => Source -> Sem r Source
findRoot = fix $ \next s -> fetchSource s >>= maybe (return s) next
```

