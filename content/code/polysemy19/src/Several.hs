{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Several where

import Data.Kind
import Polysemy
import Polysemy.Internal.Kind (Append)
import Polysemy.Reader
import Prelude

infixr 5 :::

data HList a where
  HNil :: HList '[]
  (:::) :: a -> HList (b :: [Type]) -> HList (a ': b)

type family TypeMap (f :: a -> b) (xs :: [a]) where
  TypeMap _ '[] = '[]
  TypeMap f (x ': xs) = f x ': TypeMap f xs

type family TypeConcat (a :: [t]) (b :: [t]) where
  TypeConcat '[] b = b
  TypeConcat (a ': as) b = a ': TypeConcat as b

class Several t c where
  runSeveral ::
    c r =>
    (forall r' k x. c r' => k -> Sem (e k ': r') x -> Sem r' x) ->
    HList t ->
    Sem (Append (TypeMap e t) r) a ->
    Sem r a

instance Several '[] c where
  runSeveral _ _ = id

instance Several ts c => Several (t ': ts) c where
  runSeveral f (a ::: as) = runSeveral f as . f a

-- runSeveral ::
--   (forall r' k x. k -> Sem (e k ': r') x -> Sem r' x) ->
--   HList t ->
--   Sem (Append (TypeMap e t) r) a ->
--   Sem r a
-- runSeveral f (a ::: as) = runSeveral f as . f a
-- runSeveral _ HNil = id

-- runSeveral ::
--   (Members '[Embed IO] r) =>
--   (forall r' k x. (Members '[Embed IO] r') => k -> Sem (e k ': r') x -> Sem r' x) ->
--   HList t ->
--   Sem (TypeConcat (TypeMap e t) r) a ->
--   Sem r a
-- runSeveral f (a ::: as) = runSeveral f as . f a
-- runSeveral _ HNil = id
--
-- -- runSeveral ::
-- --   forall extraConstraint e t a r.
-- --   extraConstraint r =>
-- --   (forall r' k x. extraConstraint r' => k -> Sem (e k ': r') x -> Sem r' x) ->
-- --   HList t ->
-- --   Sem (TypeConcat (TypeMap e t) r) a ->
-- --   Sem r a
-- -- runSeveral f (a ::: as) = runSeveral @extraConstraint @e f as . f a
-- -- runSeveral _ HNil = id
-- --
-- -- runSeveralReaders :: (Members '[Embed IO] r) => Sem (Reader Integer : Reader Integer : r) a -> Sem (Embed IO : Embed IO : r) a
-- -- runSeveralReaders = runSeveral (\p -> runReaderWithLog p . raiseUnder @(Embed IO)) (1 ::: 2 ::: HNil)
--
runReaderWithLog :: (Members '[Embed IO] r) => i -> Sem (Reader i : r) a -> Sem r a
runReaderWithLog i sem = do
  result <- runReader i sem
  embed $ putStrLn "hello"
  return result

runSeveralReaders :: (Members '[Embed IO] r) => Sem (Reader Integer : Reader Integer : r) a -> Sem r a
runSeveralReaders = runSeveral runReaderWithLog (1 ::: 2 ::: HNil)
