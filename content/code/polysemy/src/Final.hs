module Final where

import Polysemy

main :: IO ()
main =
  runFinal $ embedFinal $ putStrLn "Hello, world!"
