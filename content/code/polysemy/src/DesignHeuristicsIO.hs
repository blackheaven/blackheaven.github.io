{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsIO where

import Control.Concurrent (threadDelay)
import Data.Kind
import Polysemy
import Polysemy.Trace

data Pause (m :: Type -> Type) a where
  Pause :: Int -> Pause m ()

makeSem ''Pause

runPause :: Member (Embed IO) r => InterpreterFor Pause r
runPause =
  interpret $
    \case
      Pause x -> embed $ threadDelay x

runPauseAltered :: Member (Embed IO) r => (Int -> Int) -> InterpreterFor Pause r
runPauseAltered n =
  interpret $
    \case
      Pause x -> embed $ threadDelay $ n x

actEmbed :: Members '[Embed IO] r => Sem r ()
actEmbed = do
  embed $ putStrLn "Do stuff"
  embed $ threadDelay 250
  embed $ putStrLn "Do even more stuff"

actLifted :: Members '[Trace, Pause] r => Sem r ()
actLifted = do
  trace "Do stuff"
  pause 250
  trace "Do even more stuff"

main :: IO ()
main = do
  runM actEmbed
  runM $ traceToStdout $ runPause actLifted
  runM $ traceToStdout $ runPauseAltered (const 1000) actLifted
