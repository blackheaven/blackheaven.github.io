+++
title = "Polysemy: Design heuristics: Hiding vs Exposing"
date = 2023-05-14
draft = false
path = "2023-05/polysemy-design-hiding-vs-exposing"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

When it comes to interpreters relying on other effects, you can either hide or expose them.

For example, last year we introduced a [Cache effect](@/2022-12-18_polysemy-interpretation-effects-inline-injection.md):

```haskell
data Cache k v (m :: Type -> Type) a where
  Cached :: k -> Cache k v m v

makeSem ''Cache

runCache :: forall k v r. Ord k => (k -> Sem r v) -> InterpreterFor (Cache k v) r
runCache f =
  evalState @(M.Map k v) mempty
    . reinterpret
      ( \case
          Cached k -> do
            currentCache <- get @(M.Map k v)
            case currentCache M.!? k of
              Nothing -> do
                v <- raise $ f k
                put $ M.insert k v currentCache
                return v
              Just v -> return v
      )
```

The main motivation are:

* We don't want another interpreter to interact on it, breaking effect's logic
* `State` is only used locally

Note that you may want to expose it:

* If you want to test it, but exposing `State` will create interpretation-coupled tests (brittle), you can leverage [interceptors](@/2022-12-25_polysemy-interceptors-intro.md) instead
* If you want to add more operation (for example a `Purge` operation in another effect), you can use [`InterpretersFor`](@/2023-04-05_polysemy-design-heuristics-grouping-interpreters.md)

On another hand, you have effects relying on other monads, such as a Document relying on BloodHound, or multiples effects relying on a single one:

```haskell
data DocumentEffect d (m :: Type -> Type) a where
  CreateDocument :: d -> DocumentEffect d m Id
  UpdateDocument :: Id -> (d -> d) -> DocumentEffect d m ()

makeSem ''DocumentEffect

interpreterBH :: forall d r. Member (Embed BH) r => IndexName -> InterpreterFor (DocumentEffect d) r
interpreterBH index =
  interpret $
    \case
      CreateDocument doc -> embed @BH $ BH.indexDocument index doc
      UpdateDocument docId f -> embed @BH $ BH.updateDocument index docId f
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsHidingExposing.hs).

