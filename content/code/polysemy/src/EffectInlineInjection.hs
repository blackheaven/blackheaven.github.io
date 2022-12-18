{-# LANGUAGE TemplateHaskell #-}

module EffectInlineInjection
  ( effectInlineInjection,
  )
where

import qualified Data.Map.Strict as M
import Data.Kind
import Polysemy
import Polysemy.State

data Cache k v (m :: Type -> Type) a where
  Cached :: k -> Cache k v m v

makeSem ''Cache

runCache :: forall k v r. Ord k => (k -> Sem r v) -> InterpreterFor (Cache k v) r
runCache f =
  evalState mempty
  . interpret (\case
        Cached k -> do
          currentCache <- get @(M.Map k v)
          case currentCache M.!? k of
            Nothing -> do
              v <- raise $ f k
              put $ M.insert k v currentCache
              return v
            Just v -> return v
      )
  . raiseUnder @(State (M.Map k v))

app :: Members '[Embed IO, Cache String String] r => Sem r ()
app = do
  name0 <- cached "name"
  embed $ putStrLn $ "Your name is " <> name0
  location0 <- cached "location"
  embed $ putStrLn $ "Your location is " <> location0
  name1 <- cached "name"
  location1 <- cached "location"
  embed $ putStrLn $ "Hello " <> name1 <> " from " <> location1

intrApp :: Sem '[Cache String String, Embed IO] a -> IO a
intrApp =
  runM
    . runCache (\k -> embed $ putStrLn ("What's your " <> k <> "?") >> getLine)

effectInlineInjection :: IO ()
effectInlineInjection = intrApp app

