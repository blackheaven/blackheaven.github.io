+++
title = "Polysemy: Changing return"
date = 2023-02-26
draft = false
path = "2023-02/polysemy-changing-return"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Polysemy provides some interpreters changing the returned value:

```haskell
runWriter :: Monoid o => Sem (Writer o ': r) a -> Sem r (o, a)
```

The easiest way to acheive that is to use `State`:

```haskell
runWriter
    :: Monoid o
    => Sem (Writer o ': r) a
    -> Sem r (o, a)
runWriter = runState mempty . reinterpretH intr
```

