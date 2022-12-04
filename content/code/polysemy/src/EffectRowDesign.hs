module EffectRowDesign
  ( displayFile,
    intrDisplayFile,
    mainDisplayFile,
  )
where

import Polysemy
import Polysemy.Trace

type AppEffects = '[Trace, Embed IO]

displayFile :: Members AppEffects r => FilePath -> Sem r Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content

intrDisplayFile :: Sem AppEffects a -> IO a
intrDisplayFile =
  runM
    . traceToStdout

mainDisplayFile :: IO ()
mainDisplayFile = intrDisplayFile (displayFile "/tmp/hello.txt") >>= print
