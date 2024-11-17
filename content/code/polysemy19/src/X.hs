{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module X where

import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Constraint
import Data.Kind
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Text (Text)
import Polysemy

class OneOf (encoder :: Type -> Constraint) (es :: [Type]) e where
  getDict :: Proxy es -> Proxy e -> Dict (encoder e)

instance {-# OVERLAPPING #-} (encoder e) => OneOf encoder (e ': es) e where
  getDict _ _ = Dict

instance (OneOf encoder es a) => OneOf encoder (e ': es) a where
  getDict _ _ = getDict (Proxy @es) Proxy

data ReadEvents (encoder :: Type -> Constraint) (es :: [Type]) (m :: Type -> Type) a where
  ReadEvents :: (OneOf encoder es e) => Proxy e -> StreamId -> s -> (s -> e -> s) -> ReadEvents encoder es m s

data StreamId = MainStream | NamedStream Text

makeSem ''ReadEvents

-- * Fake event store

getStream :: StreamId -> [ByteString]
getStream MainStream = ["1", "2", "3"]
getStream (NamedStream "foo") = ["\"a\"", "\"b\"", "\"c\""]
getStream (NamedStream _) = []

runFakeEventStore :: forall es a r. Sem (ReadEvents FromJSON es ': r) a -> Sem r a
runFakeEventStore = interpret \case
  ReadEvents p stream s fold ->
    withDict (getDict @FromJSON (Proxy @es) p) $
      pure $
        foldl fold s $
          mapMaybe decode $
            getStream stream

addAll :: (OneOf encoder es Int, Member (ReadEvents encoder es) r) => Sem r String
addAll = show <$> readEvents (Proxy @Int) MainStream 0 (+)

dashes :: (OneOf encoder es Char, Member (ReadEvents encoder es) r) => Sem r String
dashes = readEvents Proxy (NamedStream "foo") "" \str char -> str <> ['-', char]

main :: IO ()
main = do
  putStrLn $
    run $
      runFakeEventStore @'[Int, Char] $ do
        str1 <- addAll
        str2 <- dashes
        pure $ str1 <> str2
