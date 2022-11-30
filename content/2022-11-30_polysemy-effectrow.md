+++
title = "Polysemy: EffectRow"
date = 2022-11-30
draft = false
path = "2022-11/polysemy-effectrow"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

As seen in the [previous log](@/2022-11-27_polysemy-interpreters-intro.md) we ended up with:

```haskell
import Polysemy
import Polysemy.Trace

displayFile :: FilePath -> Sem '[Trace, Embed IO] Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content

intrDisplayFile :: Sem '[Trace, Embed IO] a -> IO a
intrDisplayFile =
  runM
    . traceToStdout
```

As you can see `'[Trace, Embed IO]` is repeated twice:

* During expression definition
* During interpretation

It's actually an [`EffectRow`](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy.html#t:EffectRow) which represents a type-level list of `Effect`.

This duplication looks nice but would be impractical.

For example, switching order in the expression:

```haskell
displayFile :: FilePath -> Sem '[Embed IO, Trace] Int
```

Would create an error:

```
src/Intro.hs:24:36-63: error:
    • Couldn't match type: Embed IO
                     with: Trace
      Expected: Sem '[Trace, Embed IO] Int
        Actual: Sem '[Embed IO, Trace] Int
    • In the first argument of ‘intrDisplayFile’, namely
        ‘(displayFile "/tmp/hello.txt")’
      In the first argument of ‘(>>=)’, namely
        ‘intrDisplayFile (displayFile "/tmp/hello.txt")’
      In the expression:
        intrDisplayFile (displayFile "/tmp/hello.txt") >>= print
   |
24 | mainDisplayFile = intrDisplayFile (displayFile "/tmp/hello.txt") >>= print
   |                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

We would have the same issue calling it from another function:

```haskell
displayFile' :: FilePath -> Sem '[Embed IO, Trace] Int
displayFile' = displayFile
```

```
src/Intro.hs:24:16-26: error:
    • Couldn't match type: Trace
                     with: Embed IO
      Expected: FilePath -> Sem '[Embed IO, Trace] Int
        Actual: FilePath -> Sem '[Trace, Embed IO] Int
    • In the expression: displayFile
      In an equation for ‘displayFile'’: displayFile' = displayFile
   |
24 | displayFile' = displayFile
   | 
```

Actually `'[Trace, Embed IO]` is a _concrete_ `EffectRow`.

There is also a `constrained` `EffectRow`, which is defined like this:

```haskell
displayFile :: Members '[Trace, Embed IO] r => FilePath -> Sem r Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content
```

we can also define them one by one:

```haskell
displayFile' :: (Member Trace r, Member (Embed IO) r) => FilePath -> Sem r Int
displayFile' = displayFile
```

in these example, function definitonsare unchanged.

However the `EffectRow` is now polymorphic and has constraints attached to it.

Now we are able to call any function with other `EffectRow` (as long as it fits the constraints).

While _concrete_ `EffectRow` is mandatory at interpretation time, during expression building it's limiting.

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/EffectRow.hs).
