{-# LANGUAGE RecursiveDo #-}

module Fixpoint where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Map.Strict as Map
import Data.Maybe
import Polysemy
import Polysemy.Fixpoint
import Polysemy.Reader

type Source = String

type Sources = Map.Map Source Source

sources :: Sources
sources =
  Map.fromList
    [ ("4", "3"),
      ("3", "2"),
      ("2", "1"),
      ("1", "0")
    ]

fetchSource :: Members '[Reader Sources, Final IO] r => Source -> Sem r (Maybe Source)
fetchSource s = do
  embedFinal $ putStrLn $ "Asked: " <> show s
  Map.lookup s <$> ask

findRoot :: Members '[Reader Sources, Fixpoint, Final IO] r => Source -> Sem r Source
findRoot = fix $ \next s -> fetchSource s >>= maybe (return s) next

findRoute4Path :: Members '[Reader Sources, Fixpoint, Final IO] r => Source -> Sem r Source
findRoute4Path i = mdo
  let fetch x = fromMaybe i <$> fetchSource x
      path = init $ concatMap (<> ".") [s0, s1, s2, s3]
  s0 <- fetch i
  s1 <- fetch s0
  s2 <- fetch s1
  s3 <- fetch s2
  return path

data Widget = Widget
  { name :: String,
    observers :: [Widget]
  }

moveWidget :: Members '[Final IO] r => Widget -> Sem r ()
moveWidget w =
  embedFinal $ do
    putStrLn $ "Moving " <> w.name <> " done"
    forM_ w.observers $ \o ->
      putStrLn $ "Moving " <> w.name <> " notify " <> o.name

newWidget :: String -> Widget -> Sem r Widget
newWidget n w = return $ Widget n [w]

observations :: Members '[Fixpoint, Final IO] r => Sem r ()
observations = mdo
  marco <- newWidget "Marco" polo
  polo <- newWidget "Polo" marco
  moveWidget marco
  moveWidget polo

main :: IO ()
main = do
  root <- runFinal $ fixpointToFinal @IO $ runReader sources $ findRoot "4"
  print root
  root' <- runFinal $ fixpointToFinal @IO $ runReader sources $ findRoute4Path "4"
  print root'
  runFinal $ fixpointToFinal @IO observations
