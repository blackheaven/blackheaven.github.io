module EffectRow
  ( displayFile,
    displayFile',
    intrDisplayFile,
    mainDisplayFile,
  )
where

import Polysemy
import Polysemy.Trace

displayFile :: Members '[Trace, Embed IO] r => FilePath -> Sem r Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content

displayFile' :: (Member Trace r, Member (Embed IO) r) => FilePath -> Sem r Int
displayFile' = displayFile

intrDisplayFile :: Sem '[Trace, Embed IO] a -> IO a
intrDisplayFile =
  runM
    . traceToStdout

mainDisplayFile :: IO ()
mainDisplayFile = intrDisplayFile (displayFile "/tmp/hello.txt") >>= print
