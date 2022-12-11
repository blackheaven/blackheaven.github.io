+++
title = "Polysemy: Introduction to Effect interpretation"
date = 2022-12-11
draft = false
path = "2022-12/polysemy-effect-interpretation-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In my [last log](@/2022-12-07_polysemy-effect-definition-intro.md) we defined the `Trace` effect:

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

One of the main interest of effects systems is to be able to define multiple interpretations for the same effects.

The simplest interpreter we can write is to drop simply the action:

```haskell
ignoreTrace :: Sem (Trace ': r) a -> Sem r a
ignoreTrace = interpret $ \case
  Trace _ -> pure ()
```

`interpret` will "consume" `Trace` contructor-by-constructor.

Note: we have a useful type alias `InterpreterFor` for interpreters definitions.

However, `Trace` is simple enough to have a dropping interpreter, let's come with a more useful definition:

```haskell
traceToStdout :: Member (Embed IO) r => InterpreterFor Trace r
traceToStdout = interpret $ \case
  Trace m -> embed $ putStrLn m
```

We can also compose interpreters:

```haskell
traceToStderr :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToStderr = traceToHandle stderr

traceToHandle :: Member (Embed IO) r => Handle -> Sem (Trace ': r) a -> Sem r a
traceToHandle handle = interpret $ \case
  Trace m -> embed $ hPutStrLn handle m
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/EffectInterpretationIntro.hs).
