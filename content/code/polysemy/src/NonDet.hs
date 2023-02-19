module NonDet where

import Control.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.NonDet

failing :: Members '[Error Integer, Embed IO] r => Sem r Integer
failing = do
  embed $ putStrLn "failing"
  throw @Integer 1

working0 :: Members '[Error Integer, Embed IO] r => Sem r Integer
working0 = do
  embed $ putStrLn "working0"
  return 42

working1 :: Members '[Error Integer, Embed IO] r => Sem r Integer
working1 = do
  embed $ putStrLn "working1"
  return 42

actFailingFirst :: Members '[Error Integer, NonDet, Embed IO] r => Sem r Integer
actFailingFirst = failing <|> working0 <|> working1

actWorkingFirst :: Members '[Error Integer, NonDet, Embed IO] r => Sem r Integer
actWorkingFirst = working0 <|> failing <|> working1

main :: IO ()
main = do
  putStrLn "# FailingFirst"
  putStrLn "nonDetToError"
  runM (runError @Integer $ nonDetToError @Integer 2 actFailingFirst) >>= print
  putStrLn "## runNonDet"
  runM (runError @Integer $ runNonDet @Maybe actFailingFirst) >>= print
  putStrLn "## runNonDetMaybe"
  runM (runError @Integer $ runNonDetMaybe actFailingFirst) >>= print
  putStrLn "# WorkingFirst"
  putStrLn "nonDetToError"
  runM (runError @Integer $ nonDetToError @Integer 2 actWorkingFirst) >>= print
  putStrLn "## runNonDet"
  runM (runError @Integer $ runNonDet @Maybe actWorkingFirst) >>= print
  putStrLn "## runNonDetMaybe"
  runM (runError @Integer $ runNonDetMaybe actWorkingFirst) >>= print
