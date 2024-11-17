+++
title = "Polysemy: Input"
date = 2023-01-04
draft = false
path = "2023-01/polysemy-input"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Whenever you might have varying strategies to fetch some data, you might want to use `Input`:

```haskell
data Input i m a where
  Input :: Input i m i
```

it comes with three interpreters:

* `runInputConst` to provide an unvarying value

```haskell
runInputConst :: i -> Sem (Input i ': r) a -> Sem r a
runInputConst c = interpret $ \case
  Input -> pure c
```

usable as:

```haskell
runInputConst @String "Hello, world!" $ input >>= embed . putStrLn
```

* `runInputList` which aims to give a constant list of values one by one

```haskell
runInputList ::
  [i] ->
  Sem (Input (Maybe i) ': r) a ->
  Sem r a
runInputList is =
  evalState is
    . reinterpret
      ( \case
          Input -> do
            s <- gets uncons
            for_ s $ put . snd
            pure $ fst <$> s
      )
```

usable as:

```haskell
    runInputList @Char "Hello" $ whileJust (input @(Maybe Char)) $ embed . print
```

* `runInputSem` to dynamically provide values (which should have been used instead of `View` in [a previous log](@/blog/2022-12-14_polysemy-interpretation-effects-injection.md))

```haskell
runInputSem :: forall i r a. Sem r i -> Sem (Input i ': r) a -> Sem r a
runInputSem m = interpret $ \case
  Input -> m
```

usable as:

```haskell
message <- embed $ newMVar @String "Hello, world!"
runInputSem (embed $ tryTakeMVar message) $ whileJust (input @(Maybe String)) $ embed . putStrLn
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Input.hs).
