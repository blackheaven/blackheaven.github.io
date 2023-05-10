{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsDispatching where

import Data.Kind
import Polysemy

data DocumentsEffects (d :: Type) (m :: Type -> Type) (a :: Type) where
  StoreDocument :: d -> DocumentsEffects d m ()
  ListDocuments :: DocumentsEffects d m [d]

makeSem ''DocumentsEffects

data Log = Log

data Recipe = Recipe

data Bill = Bill

data DocumentType a where
  DTLog :: DocumentType Log
  DTReciepe :: DocumentType Recipe
  DTBill :: DocumentType Bill

data AnyDocumentsEffects (m :: Type -> Type) (a :: Type) where
  AnyStoreDocument :: DocumentType d -> d -> AnyDocumentsEffects m ()
  AnyListDocuments :: DocumentType d -> AnyDocumentsEffects m [d]

makeSem ''AnyDocumentsEffects

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
