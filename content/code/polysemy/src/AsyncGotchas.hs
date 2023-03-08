module AsyncGotchas where

import Control.Concurrent (threadDelay)
import Polysemy
import Polysemy.Async
import Polysemy.Error

failing :: Members '[Error (), Embed IO] r => Sem r Int
failing = do
  embed $ putStrLn "Failing"
  throw ()

actFailing :: Members '[Error (), Embed IO, Async] r => Sem r ()
actFailing = do
  a <- async failing
  embed $ threadDelay 250
  embed $ putStrLn "Do stuff"
  r <- await a
  embed $ putStrLn "Do even more stuff"
  embed $ print r

working :: Members '[Error (), Embed IO] r => Sem r Int
working = do
  embed $ putStrLn "Working"
  return 42

actWorking :: Members '[Error (), Embed IO, Async] r => Sem r ()
actWorking = do
  a <- async working
  embed $ threadDelay 250
  embed $ putStrLn "Do stuff"
  r <- await a
  embed $ putStrLn "Do even more stuff"
  embed $ print r

main :: IO ()
main = do
  runM (runError @() $ asyncToIO actFailing) >>= print
  threadDelay 250
  runM (runError @() $ asyncToIO actWorking) >>= print
