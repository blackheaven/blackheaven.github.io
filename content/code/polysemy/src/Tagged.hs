module Tagged
( taggedMainSplit,
  taggedMainJoined,
)
where

import Polysemy
import Polysemy.Reader
import Polysemy.Tagged

displayName :: Members '[Reader String, Embed IO] r => Sem r ()
displayName = do
  name <- ask
  embed $ putStrLn $ "Your name is " <> name

displayLocation :: Members '[Reader String, Embed IO] r => Sem r ()
displayLocation = do
  location <- ask
  embed $ putStrLn $ "Your location is " <> location

displaySplit :: Members '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] r => Sem r ()
displaySplit = do
  tag @"name" @(Reader String) displayName
  tag @"location" @(Reader String) displayLocation

displayJoined :: Members '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] r => Sem r ()
displayJoined = do
  name <- tag @"name" @(Reader String) ask
  location <- tag @"location" @(Reader String) ask
  embed $ putStrLn $ "Your name is " <> name
  embed $ putStrLn $ "Your location is " <> location

runApp :: Sem '[Tagged "name" (Reader String), Tagged "location" (Reader String), Embed IO] a -> IO a
runApp =
  runM
  . runReader "France"
  . untag @"location"
  . runReader "Gautier"
  . untag @"name"

taggedMainSplit :: IO ()
taggedMainSplit = runApp displaySplit

taggedMainJoined :: IO ()
taggedMainJoined = runApp displayJoined
