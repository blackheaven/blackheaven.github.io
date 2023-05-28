{-# LANGUAGE TemplateHaskell #-}

module Scoped where

import Control.Concurrent
import Control.Monad (void)
import Data.Kind
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Resource
import Polysemy.Scoped

data Queue q (m :: Type -> Type) a where
  Dequeue :: Queue q m (Maybe q)

makeSem ''Queue

data Stack q (m :: Type -> Type) a where
  Pop :: Stack q m (Maybe q)

makeSem ''Stack

works :: Members '[Scoped (Maybe ()) (Queue ()), Error (), Embed IO] r => Sem r ()
works =
  scoped @_ @(Queue ()) (Just ()) $ do
    void $ dequeue @()
    void $ dequeue @()

fails :: Members '[Scoped (Maybe ()) (Queue ()), Error (), Embed IO] r => Sem r ()
fails =
  scoped @_ @(Queue ()) (Just ()) $ do
    void $ dequeue @()
    () <- throw ()
    void $ dequeue @()

withQueue ::
  Members '[Embed IO] r =>
  Maybe q ->
  (MVar q -> Sem r a) ->
  Sem r a
withQueue initial use = do
  mvar <- embed $ maybe newEmptyMVar newMVar initial
  use mvar

interpretQueue ::
  forall q r.
  Members '[Embed IO] r =>
  InterpreterFor (Scoped (Maybe q) (Queue q)) r
interpretQueue =
  interpretScopedWith @'[] withQueue $ \mvar -> \case
    Dequeue -> do
      embed $ putStrLn "deqeue"
      embed $ tryTakeMVar mvar

main :: IO ()
main = do
  putStrLn "Work"
  runM (runError @() $ interpretQueue @() works) >>= print
  putStrLn "Fail"
  runM (runError @() $ interpretQueue @() fails) >>= print

withQueue' ::
  Members '[Embed IO] r =>
  Maybe q ->
  (() -> Sem (Stack q : r) a) ->
  Sem r a
withQueue' initial use = do
  mvar <- embed $ maybe newEmptyMVar newMVar initial
  runStackMVar mvar $ use ()

runStackMVar :: Members '[Embed IO] r => MVar q -> InterpreterFor (Stack q) r
runStackMVar mvar =
  interpret $
    \case
      Pop -> embed $ tryTakeMVar mvar

interpretQueue' ::
  forall q r.
  Members '[Embed IO] r =>
  InterpreterFor (Scoped (Maybe q) (Queue q)) r
interpretQueue' =
  interpretScopedWith @'[Stack q] withQueue' $ \() -> \case
    Dequeue -> do
      embed $ putStrLn "deqeue"
      pop
