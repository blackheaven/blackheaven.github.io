module AtomicState where

import Control.Monad
import Data.IORef
import Polysemy
import Polysemy.Async
import Polysemy.AtomicState
import Polysemy.State

incHundred :: Members '[State Int] r => Sem r ()
incHundred =
  forM_ ([0 .. 99] :: [Int]) $ \_ ->
    modify' @Int (+ 1)

incThousand :: Members '[State Int, Async] r => Sem r ()
incThousand =
  void $ sequenceConcurrently $ map (const incHundred) ([0 .. 9] :: [Int])

incHundred' :: Members '[AtomicState Int] r => Sem r ()
incHundred' =
  forM_ ([0 .. 99] :: [Int]) $ \_ ->
    atomicModify' @Int (+ 1)

incThousand' :: Members '[AtomicState Int, Async] r => Sem r ()
incThousand' =
  void $ sequenceConcurrently $ map (const incHundred') ([0 .. 9] :: [Int])

main :: IO ()
main = do
  result <- runM $ asyncToIO $ runState @Int 0 incThousand
  print result
  cntr <- newIORef @Int 0
  result' <- runM $ asyncToIO $ runAtomicStateIORef cntr incThousand'
  print result'
