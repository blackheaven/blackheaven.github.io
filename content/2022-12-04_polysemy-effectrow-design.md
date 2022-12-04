+++
title = "Polysemy: EffectRow design"
date = 2022-12-04
draft = false
path = "2022-12/polysemy-effectrow-design"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[Previous log](@/2022-11-30_polysemy-effectrow.md) introduced [`EffectRow`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy.html#t:EffectRow).

It could be tempting to create a `type` and apply it everywhere in the codebase as follows:

```haskell
type AppEffects = '[Trace, Embed IO]

displayFile :: Members AppEffects r => FilePath -> Sem r Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content

intrDisplayFile :: Sem AppEffects a -> IO a
intrDisplayFile =
  runM
    . traceToStdout
```

While it sounds to be a way to save time, it:

* Obfuscate expression's type
* Reduce reusability
* Can hide potentila implementation issues

We can argue that specifying by hand, only necessary effect can be cumbersome (especially we you add an effect in a big stack call), it's actually a strong hint that you're probably doing something you should be thoughtful about.

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/EffectRowDesign.hs).
