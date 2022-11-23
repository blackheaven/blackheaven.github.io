module Intro
  ( displayFile,
    intrDisplayFile,
    mainDisplayFile,
  )
where

import Polysemy
import Polysemy.Trace

displayFile :: FilePath -> Sem '[Trace, Embed IO] Int
displayFile path = do
  trace $ "Displaying " <> path
  content <- embed $ readFile path
  embed $ putStr content
  return $ length content

intrDisplayFile :: Sem '[Trace, Embed IO] a -> IO a
intrDisplayFile =
  runM
    . traceToStdout

mainDisplayFile :: IO ()
mainDisplayFile = intrDisplayFile (displayFile "/tmp/hello.txt") >>= print
