+++
title = "Polysemy: An introduction"
date = 2022-11-23
draft = false
path = "2022-11/polysemy-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

I used to teach functional programming using Haskell in an engineering.

I was starting my course by defining functional programming being defined by three interdependent components:

* Types
* Expressions
* Interpretations

I was emphasizing Haskell's precise type system, and all went well until they encountered `IO`.

Haskell provides a lot of tools to create a fine grained design for pure computations (with (G)ADTs, types synonyms, etc.), but when it comes to I/O, we have one generic type.

While it may simplify my course, it's not helpful for my production-level products.

A way to mitigate that is to use _effects systems_, which are design to have small, well-defined effects, grouped in typed.

A widespread implementation of this mechanism is the [`mtl`](http://book.realworldhaskell.org/read/monad-transformers.html), but it has [a number of issues](https://ro-che.info/articles/2014-06-11-problem-with-mtl) (and [here](https://github.com/haskell-effectful/effectful/blob/master/transformers.md)).

Since September 2020 I'm using [Polysemy](https://hackage.haskell.org/package/polysemy) ([see my feedback one year later](https://blog.hetchr.com/polysemy-retrospective/)).

This log is the first one of a long series going through Polysemy.

I'll use Polysemy `1.7.1.0` with GHC `9.2.5`, I'll drop the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy).

Let's start with a simple example:

```haskell
import Polysemy
import Polysemy.Trace

displayFile :: FilePath -> Sem '[Trace, Embed IO] Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content
```

We define a function `displayFile` which produce a `Sem` expression which:

* Produces an `Int` (the file size) once interpreted
* Uses `Trace` and `Embed IO` (a "lifted" `Monad`) effects (`'[Trace, Embed IO]` is an `EffectRow`)

Step by step we do the following things:

* Emit a `Trace` effect with `trace`
* Emit an `Embed IO` effect with `embed` of `readFile`
* Emit an `Embed IO` effect with `embed` of `putStr`
* Produce the `content`'s length

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Intro.hs).
