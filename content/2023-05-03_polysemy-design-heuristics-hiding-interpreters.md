+++
title = "Polysemy: Design heuristics: Hiding interpreters"
date = 2023-05-03
draft = false
path = "2023-05/polysemy-design-heuristics-hiding-interpreters"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In the codebases I have worked on all have multiple global interpreters, most noticeably for production and tests.

From time to time, you want to have the same effect lists, for example to dynamically change the interpretation.

To do so you can inject effects:

Let's take back our `Pause` effect:

```haskell
data Pause (m :: Type -> Type) a where
  Pause :: Int -> Pause m ()

makeSem ''Pause
```

We can have two interpreters:

```haskell
runPause :: Member (Embed IO) r => InterpreterFor Pause r
runPause =
  interpret $
    \case
      Pause x -> embed $ threadDelay x

runPause :: Member (Embed IO) r => InterpreterFor Pause r
runPause =
  interpret $
    \case
      Pause x -> embed $ threadDelay x
```

Then we can have two global interpreters:

```haskell
interpretEmbed :: Sem '[Pause] a -> IO a
interpretEmbed = runM . runPauseEmbed . raiseUnder @(Embed IO)

interpretFinal :: Sem '[Pause] a -> IO a
interpretFinal = runFinal . runPauseFinal . raiseUnder @(Final IO)
```

`raiseUnder` injects one effect under the current head.

There are plenty of other functions such as `raiseUnder2` and `raiseUnder3`, which adds 2/3 effects under the head, `raise2Under`/`raise3Under` which adds an effect in 2nd/3rd position under the effects' head.

More generally `raise_`/`insertAt` can helps you to acheive that with more flexibility.

Finally `subsume_` will give you the power to rewrite your effects list (effects order, duplicate or merge effects).

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsHidingInterpreters.hs).

