+++
title = "Polysemy: Design heuristics: Dispatcher"
date = 2023-05-10
draft = false
path = "2023-05/polysemy-design-heuristics-dispatchers"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

At some point you may have strongly typed effects:

```haskell
data DocumentsEffects (d :: Type) (m :: Type -> Type) (a :: Type) where
  StoreDocument :: d -> DocumentsEffects d m ()
  ListDocuments :: DocumentsEffects d m [d]

makeSem ''DocumentsEffects
```

Sounds nice, until you might want to dynamically dispatch multiples types.

In order to do so, we have to rely on a GADT:

```haskell
data DocumentType a where
  DTLog :: DocumentType Log
  DTReciepe :: DocumentType Recipe
  DTBill :: DocumentType Bill
```

Then, we need a dedicated effect:

```haskell
data AnyDocumentsEffects (m :: Type -> Type) (a :: Type) where
  AnyStoreDocument :: DocumentType d -> d -> AnyDocumentsEffects m ()
  AnyListDocuments :: DocumentType d -> AnyDocumentsEffects m [d]

makeSem ''AnyDocumentsEffects
```

Finally, you have to prove that each effect is present:

```haskell
interpretAnyDocumentsEffects :: Members '[DocumentsEffects Log, DocumentsEffects Recipe, DocumentsEffects Bill] r => InterpreterFor AnyDocumentsEffects r
interpretAnyDocumentsEffects =
  interpret $
    \case
      AnyStoreDocument DTLog x -> storeDocument x
      AnyStoreDocument DTReciepe x -> storeDocument x
      AnyStoreDocument DTBill x -> storeDocument x
      AnyListDocuments DTLog -> listDocuments @Log
      AnyListDocuments DTReciepe -> listDocuments @Recipe
      AnyListDocuments DTBill -> listDocuments @Bill
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsDispatching.hs).

