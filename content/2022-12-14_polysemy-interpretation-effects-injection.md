+++
title = "Polysemy: Interpretation and effects injection"
date = 2022-12-14
draft = false
path = "2022-12/polysemy-interpretation-effects-injection"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

As seen previously we can have interpreters which relies on other interpreters.

While you could simply add the required effects to the list, you may want to hide it (eg. to have dynamique interpreters).

In order to do it, you have many `raise*` functions.

Let's have a look at `View`:

```haskell
data View v m a where
  See :: View v m v

viewToState :: Member (State s) r => (s -> Sem r v) -> InterpreterFor (View v) r
```

`View` gives a way to cache expensive computations dynamically.

Let's imagine a simple app:

```haskell
app :: Members '[Embed IO, View String] r => Sem r ()
app = do
  name0 <- see
  embed $ putStrLn $ "Your name is " <> name0
  name1 <- see
  embed $ putStrLn $ "Hello " <> name1
```

we can leverage `raiseUnder` which injects an effect as follows:

```haskell
intrApp :: Sem '[View String, Embed IO] a -> IO a
intrApp =
  runM
    . evalState ()
    . viewToState (\() -> embed $ putStrLn "What's your name?" >> getLine)
    . raiseUnder @(State ())
```

Here `raiseUnder` will change effects to `'[View String, State (), Embed IO]`

See how it runs:

```
What's your name?
Gautier
Your name is Gautier
Hello Gautier
```

Note: actually `viewToState` is more general and invalidate the cache once the state changes

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/content/code/polysemy/src/EffectInjection.hs).
