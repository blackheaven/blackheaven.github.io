+++
title = "Polysemy: Embed"
date = 2022-01-01
draft = false
path = "2023-01/polysemy-embed"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

While you could perfectly stay in pure computation, using already defined `Monad` (such as `IO`) is a widespread usage.

That's what `Embed` has been designed for:

```haskell
newtype Embed m (z :: Type -> Type) a where
  Embed :: { unEmbed :: m a } -> Embed m z a
```

used as follows:

```haskell
logic :: Member (Embed IO) r => Sem r ()
logic = do
  embed $ putStrLn "Hello, world!"
```

finally we have `runM`:

```haskell
runM :: Monad m => Sem '[Embed m] a -> m a
```

usable as:

```haskell
runM logic
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Embed.hs).
