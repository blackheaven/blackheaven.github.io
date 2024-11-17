+++
title = "Polysemy: Higher-order effects with Final"
date = 2023-01-15
draft = false
path = "2023-01/polysemy-hoe-final"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/blog/2023-01-08_polysemy-hoe-intro.md), we introduced _Higher-order effects_ definition and interpretation through tactics.

Some elements from tactics are already deprecated, and my guess is that it will spread until tactics will be completly removed.

According to the documentation [strategies](https://hackage.haskell.org/package/polysemy-1.7.1.0/docs/Polysemy-Internal-Strategy.html) should be choosed if possible.

Let's review our effect:

```haskell
data When (m :: Type -> Type) a where
  WhenM :: m () -> When m ()

makeSem ''When
```

Now we can define the interpreter:

```haskell
interpretWhenFinal :: forall r. Member (Final IO) r => InterpreterFor When r
interpretWhenFinal =
  interpretFinal @IO $
    \case
      WhenM act -> runS act
```

Here are the key differences:
* `interpretFinal` required a `Final m` to work
* `runS` is a general function (while `runTSimple` is a special case of `runT`)

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/HoE.hs).
