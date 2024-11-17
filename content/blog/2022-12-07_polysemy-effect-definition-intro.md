+++
title = "Polysemy: Introduction to Effect definition"
date = 2022-12-07
draft = false
path = "2022-12/polysemy-effect-definition-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In the [first log](@/blog/2022-11-23_polysemy-intro.md) we explained that in the expression:

```haskell
displayFile :: FilePath -> Sem '[Trace, Embed IO] Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content
```

`trace` is emiting an effect of `Trace`.

`trace` has the following type:

```haskell
trace :: forall r. Member Trace r => String -> Sem r ()
```

and is generated via:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module EffectDefinitionIntro
  ( Trace (..),
    trace,
  )
where

import Data.Kind
import Polysemy

data Trace (m :: Type -> Type) a where
  Trace :: String -> Trace m ()

makeSem ''Trace
```

`makeSem` will generate something like:

```haskell
trace :: forall r_a44O. Member Trace r_a44O => String -> Sem r_a44O ()
trace a = emit $ Trace a
```

So, whenever `trace` is called, the value is sent to the interpreter.

Note: to keep it short `m` will be replaced by `Sem r` during interpretation

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/EffectDefinitionIntro.hs).
