{-# LANGUAGE TemplateHaskell #-}

module EffectDefinitionIntro
  ( Trace (..),
    trace,
  )
where

import Data.Kind
import Polysemy

data Trace (m :: Type -> Type) a where
  Trace :: String -> Trace m ()

makeSem ''Trace
