{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Interceptors where

import Control.Monad (forM_)
import Data.Bifunctor
import Data.Kind
import qualified Data.Map.Strict as Map
import Polysemy
import Polysemy.State

newtype Id = Id {getId :: Int}
  deriving stock (Eq, Ord, Show)

newtype Document = Document {getDocument :: String}
  deriving stock (Eq, Ord, Show)

data DocumentEffect (m :: Type -> Type) a where
  CreateDocument :: Document -> DocumentEffect m Id
  UpdateDocument :: Id -> (Document -> Document) -> DocumentEffect m ()

makeSem ''DocumentEffect

type InMemoryState = (Map.Map Id Document, Int)

interpreterInMemory :: Member (State InMemoryState) r => InterpreterFor DocumentEffect r
interpreterInMemory =
  interpret $
    \case
      CreateDocument doc -> do
        (docs, nextId) <- get
        let newId = Id nextId
        put (Map.insert newId doc docs, nextId + 1)
        return newId
      UpdateDocument docId f ->
        modify @InMemoryState $
          first (Map.update (Just . f) docId)

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

logic :: Member DocumentEffect r => Sem r ()
logic = do
  docId <- createDocument $ Document "initial"
  updateDocument docId $ const $ Document "Updated"
  updateDocument docId $ Document . (<> "!") . (.getDocument)

logicBuffered :: Member DocumentEffect r => Sem r ()
logicBuffered = do
  docId <- createDocument $ Document "initial"
  bufferize $ do
    updateDocument docId $ const $ Document "Updated"
    updateDocument docId $ Document . (<> "!") . (.getDocument)

main :: IO ()
main = do
  print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates logic
  print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates $ bufferize logic
  print $ run $ runState @CountUpdatesState mempty $ runState @InMemoryState (mempty, 0) $ interpreterInMemory $ countUpdates logicBuffered
