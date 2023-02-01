+++
title = "Polysemy: Final vs Embed"
date = 2023-02-01
draft = false
path = "2023-02/polysemy-final-vs-embed"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/2023-01-11_polysemy-final.md), we introduce `Final` as an alternative to `Embed`.

If we look at the intents:
* `Embed` aims to lift any `Monad` to `Sem`
* `Final` aims to embed higher-order actions in the effects stack

Each of them come with an interpreter allowing to interpret an effect into  other effects of the same type.

Let's start with `Embed`.

```haskell
runEmbedded ::
  forall m1 m2 r a.
  Member (Embed m2) r =>
  (forall x. m1 x -> m2 x) ->
  InterpreterFor (Embed m1) r
```

This is pretty straightforward: for two embedded `Monad`, we have to provide a function to convert the first one into the second one.

Now, we can have a look at `Final`' equivalent:

```haskell
finalToFinal ::
  forall m1 m2 r a.
  Member (Final m2) r =>
  (forall x. m1 x -> m2 x) ->
  (forall x. m2 x -> m1 x) ->
  InterpreterFor Sem (Final m1) r
```

A bit more complex: we have to provide two functions to go back and forth between the two Monads.

It's actually due to a property highlight in `Final`'s documentation:

> Beware: Final actions are interpreted as actions of the final monad, [..] This means that any interpreter built using Final will not respect local/global state semantics based on the order of interpreters run.

See the full the code [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Embed.html) and [here](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/src/Polysemy.Final.html).
