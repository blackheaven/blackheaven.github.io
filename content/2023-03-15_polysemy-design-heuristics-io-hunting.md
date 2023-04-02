+++
title = "Polysemy: Design heuristics: IO hunting"
date = 2023-03-15
draft = false
path = "2023-03/polysemy-design-heuristics-io-hunting"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

When it come to designing effects, an efficient way to know where to start is to look for `IO` (especially in `embed`/`embedFinal`).

Let's have a look at a `IO`-oriented code:

```haskell
actEmbed :: Members '[Embed IO] r => Sem r ()
actEmbed = do
  embed $ putStrLn "Do stuff"
  embed $ threadDelay 250
  embed $ putStrLn "Do even more stuff"
```

We can reuse `Trace` and introduce `Pause` to improve it:

```haskell
actLifted :: Members '[Trace, Pause] r => Sem r ()
actLifted = do
  trace "Do stuff"
  pause 250
  trace "Do even more stuff"
```

Way better, here's our effect and the default interpreter:

```haskell
data Pause (m :: Type -> Type) a where
  Pause :: Int -> Pause m ()

makeSem ''Pause

runPause :: Member (Embed IO) r => InterpreterFor Pause r
runPause =
  interpret $
    \case
      Pause x -> embed $ threadDelay x
```

We can leverage our new effect with a new interpreter:

```haskell
runPauseAltered :: Member (Embed IO) r => (Int -> Int) -> InterpreterFor Pause r
runPauseAltered n =
  interpret $
    \case
      Pause x -> embed $ threadDelay $ n x
```

It will help us to change duration (reduce it in tests, increase it to debug, making it vary to see if something break).

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsIO.hs).

