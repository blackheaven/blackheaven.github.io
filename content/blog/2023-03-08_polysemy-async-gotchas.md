+++
title = "Polysemy: Async gotchas"
date = 2023-03-08
draft = false
path = "2023-03/polysemy-async-gotchas"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

Polysemy comes with many functions and interpreters relying on `-threaded`.

However, whenever an issue (such as an `Error`) happens:

```haskell
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

main :: IO ()
main = do
  runM (runError @() $ asyncToIO actFailing) >>= print
```

The main thread stops:

```
Failing
Left ()
Do stuff
```

Be reassured, the nominal case works:

```haskell
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
  runM (runError @() $ asyncToIO actWorking) >>= print
```

```
Working
Do stuff
Do even more stuff
Just 42
Right ()
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/AsyncGotchas.hs).

