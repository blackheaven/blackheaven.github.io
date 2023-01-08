{-# LANGUAGE TemplateHaskell #-}

module HoE where

import Data.Kind
import Polysemy
import Polysemy.Final

data When (m :: Type -> Type) a where
  WhenM :: m () -> When m ()

makeSem ''When

interpretWhenEmbed :: InterpreterFor When r
interpretWhenEmbed =
  interpretH $
    \case
      WhenM act -> runTSimple act

interpretWhenFinal :: forall r. Member (Final IO) r => InterpreterFor When r
interpretWhenFinal =
  interpretFinal @IO $
    \case
      WhenM act -> runS act

main :: IO ()
main = do
  runM $ interpretWhenEmbed $ whenM $ embed $ putStrLn "Hello,"
  runFinal $ interpretWhenFinal $ whenM $ embedFinal $ putStrLn "world!"
