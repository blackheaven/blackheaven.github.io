+++
title = "Polysemy: Interceptors to buffer"
date = 2022-12-28
draft = false
path = "2022-12/polysemy-interceptors-buffering"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In [the previous post](@/blog/2022-12-25_polysemy-interceptors-intro.md) we have introduced an effect which mimics ElasticSearch indexing:

```haskell
newtype Id = Id {getId :: Int}
  deriving stock (Eq, Ord, Show)

newtype Document = Document {getDocument :: String}
  deriving stock (Eq, Ord, Show)

data DocumentEffect (m :: Type -> Type) a where
  CreateDocument :: Document -> DocumentEffect m Id
  UpdateDocument :: Id -> (Document -> Document) -> DocumentEffect m ()
```

used as follows:

```haskell
logic :: Member DocumentEffect r => Sem r ()
logic = do
  docId <- createDocument $ Document "initial"
  updateDocument docId $ const $ Document "Updated"
  updateDocument docId $ Document . (<> "!") . (.getDocument)
```

One of the issue with this piece of code is, when run against a real ElasticSearch server, it can suffer from dirty read.

A way to mitigate that is to buffer (accumulate) calls and run them all at once, we can create an interceptor for that:

```haskell
type BufferedState = Map.Map Id (Document -> Document)

bufferize :: Members '[DocumentEffect] r => Sem r a -> Sem r a
bufferize sem = do
  (s, a) <- runState @BufferedState mempty $ acc $ raise sem
  forM_ (Map.toList s) $
    uncurry updateDocument
  return a
  where
    acc =
      intercept $
        \case
          CreateDocument doc -> createDocument doc
          UpdateDocument docId f ->
            modify @BufferedState $
              Map.alter (Just . maybe f (f .)) docId
```

then simply add it to the interpretation:

```haskell
print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates $ bufferize logic
```

which gives:

```
(fromList [(Id {getId = 0},1)],((fromList [(Id {getId = 0},Document {getDocument = "Updated!"})],1),()))
```

we can now see more precisely that the updates count has been reduced to 1:

```
fromList [(Id {getId = 0},1)]
```

While buffering that way avoid dirty reads and reduce pressure on the backend, without changing the code, it may result in consistency issue.

To mitigate that, we could simply apply the buffering locally:

```haskell
logicBuffered :: Member DocumentEffect r => Sem r ()
logicBuffered = do
  docId <- createDocument $ Document "initial"
  bufferize $ do
    updateDocument docId $ const $ Document "Updated"
    updateDocument docId $ Document . (<> "!") . (.getDocument)
```

we can drop it from the interpretation:

```haskell
print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates logicBuffered
```

which keeps the same number of updates:

```
(fromList [(Id {getId = 0},1)],((fromList [(Id {getId = 0},Document {getDocument = "Updated!"})],1),()))
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Interceptors.hs).
