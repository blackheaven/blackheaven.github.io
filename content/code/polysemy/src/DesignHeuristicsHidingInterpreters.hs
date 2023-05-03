{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsHidingInterpreters where

import Control.Concurrent (threadDelay)
import Data.Kind
import Polysemy

data Pause (m :: Type -> Type) a where
  Pause :: Int -> Pause m ()

makeSem ''Pause

runPauseEmbed :: Member (Embed IO) r => InterpreterFor Pause r
runPauseEmbed =
  interpret $
    \case
      Pause x -> embed $ threadDelay x

runPauseFinal :: Member (Final IO) r => InterpreterFor Pause r
runPauseFinal =
  interpret $
    \case
      Pause x -> embedFinal $ threadDelay x

interpretEmbed :: Sem '[Pause] a -> IO a
interpretEmbed = runM . runPauseEmbed . raiseUnder @(Embed IO)

interpretFinal :: Sem '[Pause] a -> IO a
interpretFinal = runFinal . runPauseFinal . raiseUnder @(Final IO)
