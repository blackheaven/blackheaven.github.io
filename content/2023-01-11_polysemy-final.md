+++
title = "Polysemy: Final"
date = 2023-01-11
draft = false
path = "2023-01/polysemy-final"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/2023-01-08_polysemy-hoe-intro.md), we introduced _Higher-order effects_ definition and interpretation through tactics.

We'll see in another log an alternative way to define them.

Polysemy recommends using `Final` instead of `Embed` for interpreters.

It's defined as follows:

```haskell
newtype Final m z a where
  WithWeavingToFinal
    :: ThroughWeavingToFinal m z a
    -> Final m z a
```

don't worry, it does aim to be used directly.

Hopefully, it's basic usage is identical to `Embed`:

```haskell
runFinal $ embedFinal $ putStrLn "Hello, world!"
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Final.hs).
