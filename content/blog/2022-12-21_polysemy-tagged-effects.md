+++
title = "Polysemy: Tagged effects"
date = 2022-12-21
draft = false
path = "2022-12/polysemy-tagged-effects"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

At some point you'll have a lot of effects, which is not an issue, however you might duplicated types (usually, I would advise to use `newtype`, but you might not have the hand on the consumer code).

That's why you have the `Tagged` effect:

```haskell
newtype Tagged k e m a where
  Tagged :: forall k e m a. e m a -> Tagged k e m a

tag :: forall k e r a. Member (Tagged k e) r => Sem (e ': r) a -> Sem r a
```

We can use directly:

```haskell
displayJoined :: Members '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] r => Sem r ()
displayJoined = do
  name <- tag @"name" @(Reader String) ask
  location <- tag @"location" @(Reader String) ask
  embed $ putStrLn $ "Your name is " <> name
  embed $ putStrLn $ "Your location is " <> location
```

Or to wrap existing code:

```haskell
displayName :: Members '[Reader String, Embed IO] r => Sem r ()
displayName = do
  name <- ask
  embed $ putStrLn $ "Your name is " <> name

displayLocation :: Members '[Reader String, Embed IO] r => Sem r ()
displayLocation = do
  location <- ask
  embed $ putStrLn $ "Your location is " <> location

displaySplit :: Members '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] r => Sem r ()
displaySplit = do
  tag @"name" @(Reader String) displayName
  tag @"location" @(Reader String) displayLocation
```

Then we have `untag` which unwraps the underlying effect:

```haskell
runApp :: Sem '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] a -> IO a
runApp =
  runM
  . runReader "France"
  . untag @"location"
  . runReader "Gautier"
  . untag @"name"
```

Finally, here's what it gives:

```
Your name is Gautier
Your location is France
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Tagged.hs).
