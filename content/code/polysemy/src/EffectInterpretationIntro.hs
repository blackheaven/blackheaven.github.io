module EffectInterpretationIntro where

import EffectDefinitionIntro
import Polysemy
import System.IO

ignoreTrace :: Sem (Trace ': r) a -> Sem r a
ignoreTrace = interpret $ \case
  Trace _ -> pure ()

traceToStdout :: Member (Embed IO) r => InterpreterFor Trace r
traceToStdout = interpret $ \case
  Trace m -> embed $ putStrLn m

traceToStderr :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToStderr = traceToHandle stderr

traceToHandle :: Member (Embed IO) r => Handle -> Sem (Trace ': r) a -> Sem r a
traceToHandle handle = interpret $ \case
  Trace m -> embed $ hPutStrLn handle m
