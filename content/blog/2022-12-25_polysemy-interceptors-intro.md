+++
title = "Polysemy: Introduction to Interceptors"
date = 2022-12-25
draft = false
path = "2022-12/polysemy-interceptors-intro"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

From time to time, you already have a some effects and interpreters setup, but you want to add some actions without changing the behavior.

It's one of the mechanism allowed by [Aspect Oriented Programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming).

Let's imagine an effect which mimics ElasticSearch indexing:

```haskell
newtype Id = Id {getId :: Int}
  deriving stock (Eq, Ord, Show)

newtype Document = Document {getDocument :: String}
  deriving stock (Eq, Ord, Show)

data DocumentEffect (m :: Type -> Type) a where
  CreateDocument :: Document -> DocumentEffect m Id
  UpdateDocument :: Id -> (Document -> Document) -> DocumentEffect m ()
```

we can use it as follows:

```haskell
logic :: Member DocumentEffect r => Sem r ()
logic = do
  docId <- createDocument $ Document "initial"
  updateDocument docId $ const $ Document "Updated"
  updateDocument docId $ Document . (<> "!") . (.getDocument)
```

When thinking of multiple interpreters we might think of tests (and mechanisms such as [mocks and spies](https://martinfowler.com/bliki/TestDouble.html)).

In order to do a spy which counts the number of updates we can create an interceptor:

```haskell
type CountUpdatesState = Map.Map Id Int

countUpdates :: Members '[State CountUpdatesState, DocumentEffect] r => Sem r a -> Sem r a
countUpdates =
  intercept $
    \case
      CreateDocument doc -> createDocument doc
      UpdateDocument docId f -> do
        modify @CountUpdatesState $
          Map.alter (Just . maybe 1 succ) docId
        updateDocument docId f
```

Note that we should explicitly forward calls to keep the behavior.

Then we can apply it in addition to the other interpreters:

```haskell
print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates logic
```

Giving the number of updates (2):

```
(fromList [(Id {getId = 0},2)],((fromList [(Id {getId = 0},Document {getDocument = "Updated!"})],1),()))
```

here:

```
fromList [(Id {getId = 0},2)]
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/Interceptors.hs).
