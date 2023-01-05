module Input where

import Control.Concurrent.MVar
import Data.Foldable (for_)
import Data.List (uncons)
import Polysemy
import Polysemy.Input (Input (..), input)
import Polysemy.State

runInputConst :: i -> Sem (Input i ': r) a -> Sem r a
runInputConst c = interpret $ \case
  Input -> pure c

runInputList ::
  [i] ->
  Sem (Input (Maybe i) ': r) a ->
  Sem r a
runInputList is =
  evalState is
    . reinterpret
      ( \case
          Input -> do
            s <- gets uncons
            for_ s $ put . snd
            pure $ fst <$> s
      )

runInputSem :: forall i r a. Sem r i -> Sem (Input i ': r) a -> Sem r a
runInputSem m = interpret $ \case
  Input -> m

whileJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJust fetch act = do
  fetched <- fetch
  case fetched of
    Nothing -> return ()
    Just x -> act x >> whileJust fetch act

main :: IO ()
main =
  runM $ do
    runInputConst @String "Hello, world!" $ input >>= embed . putStrLn
    runInputList @Char "Hello" $ whileJust (input @(Maybe Char)) $ embed . print
    message <- embed $ newMVar @String "Hello, world!"
    runInputSem (embed $ tryTakeMVar message) $ whileJust (input @(Maybe String)) $ embed . putStrLn
