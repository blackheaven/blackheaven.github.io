module Bundle where

import Polysemy
import Polysemy.Bundle
import Polysemy.Trace

act :: Members '[Bundle '[Trace]] r => Sem r ()
act = do
  sendBundle @Trace @'[Trace] $ trace "Hello"

intrHidden :: Sem '[Bundle '[Trace], Embed IO] a -> IO a
intrHidden = runM . traceToStdout . runBundle

intrExplicit :: Sem '[Bundle '[Trace], Trace, Embed IO] a -> IO a
intrExplicit = runM . traceToStdout . subsumeBundle

main :: IO ()
main = do
  intrHidden act
  intrExplicit act
