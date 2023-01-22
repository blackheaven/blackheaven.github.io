{-# LANGUAGE TemplateHaskell #-}

module Bind where

import Data.Kind
import Polysemy
import Polysemy.Final

data BindE (m :: Type -> Type) a where
  BindE :: m a -> (a -> m b) -> BindE m b

makeSem ''BindE

interpretBindFinal :: Member (Final IO) r => InterpreterFor BindE r
interpretBindFinal =
  interpretFinal @IO $
    \case
      BindE f g -> do
        fa <- runS f
        ff <- bindS g
        pure $ fa >>= ff

interpretBindTacticLowering :: forall r. Member (Embed IO) r => InterpreterFor BindE r
interpretBindTacticLowering =
  interpretH $
    \case
      BindE f g -> do
        ma <- runT f
        mf <- bindT g
        withLowerToIO $ \lower _ -> do
          let toIO :: Sem (BindE ': r) x -> IO x
              toIO = lower . raise . interpretBindTacticLowering
          toIO ma >>= toIO . mf

interpretBindTactic :: InterpreterFor BindE r
interpretBindTactic =
  interpretH $
    \case
      BindE f g -> do
        ma <- runT f
        mf <- bindT g
        let runHoE = raise . interpretBindTactic
        runHoE ma >>= runHoE . mf
