+++
title = "Polysemy: Higher-order effects introductions"
date = 2023-01-08
draft = false
path = "2023-01/polysemy-hoe-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

One thing that sets polysemy apart is that it allows defining _Higher-order effects_.

As _higher-order functions_, _higher-order effects_ are effects defined in terms of other effects, let's try a simple one:

```haskell
data When (m :: Type -> Type) a where
  WhenM :: m () -> When m ()

makeSem ''When
```

`m ()` is the effect (it will be replaced by `Sem _ ()`).

In order to properly interpret this effect, we can rely on [tactics](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy-Internal-Tactics.html).

```haskell
interpretWhenEmbed :: InterpreterFor When r
interpretWhenEmbed =
  interpretH $
    \case
      WhenM act -> runTSimple act
```

Two elements have to be noticed:
* `interpretH` gives a context to interpret higher-order effects
* `runTSimple` which converts `m ()` to the previously mentionned context (`m` is `Sem rInitial` and cannot be used directly)

finally, we can use it as any other interpreters:

```haskell
runM $ interpretWhenEmbed $ whenM $ embed $ putStrLn "Hello,"
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/HoE.hs).
