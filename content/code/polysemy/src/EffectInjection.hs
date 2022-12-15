module EffectInjection
  ( effectInjection,
  )
where

import Polysemy
import Polysemy.State
import Polysemy.View

app :: Members '[Embed IO, View String] r => Sem r ()
app = do
  name0 <- see
  embed $ putStrLn $ "Your name is " <> name0
  name1 <- see
  embed $ putStrLn $ "Hello " <> name1

intrApp :: Sem '[View String, Embed IO] a -> IO a
intrApp =
  runM
    . evalState ()
    . viewToState (\() -> embed $ putStrLn "What's your name?" >> getLine)
    . raiseUnder @(State ())

effectInjection :: IO ()
effectInjection = intrApp app

