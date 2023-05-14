{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsHidingExposing where

import Data.Kind
import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.State

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

newtype Id = Id {getId :: Int}
  deriving stock (Eq, Ord, Show)

newtype IndexName = IndexName {getIndexName :: String}
  deriving stock (Eq, Ord, Show)

data DocumentEffect d (m :: Type -> Type) a where
  CreateDocument :: d -> DocumentEffect d m Id
  UpdateDocument :: Id -> (d -> d) -> DocumentEffect d m ()

makeSem ''DocumentEffect

newtype BH a = BH {runBH :: IO a}
  deriving newtype (Functor, Applicative, Monad)

indexDocument :: IndexName -> d -> BH Id
indexDocument _ _ = error "-"

updateDocument' :: IndexName -> Id -> (d -> d) -> BH ()
updateDocument' _ _ = error "-"

interpreterBH :: forall d r. Member (Embed BH) r => IndexName -> InterpreterFor (DocumentEffect d) r
interpreterBH index =
  interpret $
    \case
      CreateDocument doc -> embed @BH $ indexDocument index doc
      UpdateDocument docId f -> embed @BH $ updateDocument' index docId f
