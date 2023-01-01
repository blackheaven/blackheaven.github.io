module Embed where

import Polysemy

logic :: Member (Embed IO) r => Sem r ()
logic = do
  embed $ putStrLn "Hello, world!"

main :: IO ()
main = do
  runM logic
