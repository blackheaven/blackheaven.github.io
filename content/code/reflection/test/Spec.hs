{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec (main, spec) where

import Control.Applicative
import Test.Hspec
import Type.Reflection

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decodeInline" $ do
    describe "Working" $ do
      it "Meta Timer in Meta Timer" $
        decodeInline (mkStoredEvent $ WithMeta "Sun" Started) `shouldBe` Just (WithMeta "Sun" Started)
      it "Timer in Meta Timer" $
        decodeInline (mkStoredEvent $ WithMeta "Sun" Started) `shouldBe` Just Started
      it "Timer in Timer" $
        decodeInline (mkStoredEvent Started) `shouldBe` Just Started
    describe "Not working" $ do
      it "Meta Timer in Meta Media" $
        decodeInline (mkStoredEvent $ WithMeta "Sun" Played) `shouldBe` Nothing @(WithMeta TimerEvent)
      it "Timer in Meta Media" $
        decodeInline (mkStoredEvent $ WithMeta "Sun" Played) `shouldBe` Nothing @TimerEvent
      it "Timer in Media" $
        decodeInline (mkStoredEvent Played) `shouldBe` Nothing @TimerEvent
  describe "decodeClass" $ do
    describe "Working" $ do
      it "Meta Timer in Meta Timer" $
        decodeClass (mkStoredEvent $ WithMeta "Sun" Started) `shouldBe` Just (WithMeta "Sun" Started)
      it "Timer in Meta Timer" $
        decodeClass (mkStoredEvent $ WithMeta "Sun" Started) `shouldBe` Just Started
      it "Timer in Timer" $
        decodeClass (mkStoredEvent Started) `shouldBe` Just Started
    describe "Not working" $ do
      it "Meta Timer in Meta Media" $
        decodeClass (mkStoredEvent $ WithMeta "Sun" Played) `shouldBe` Nothing @(WithMeta TimerEvent)
      it "Timer in Meta Media" $
        decodeClass (mkStoredEvent $ WithMeta "Sun" Played) `shouldBe` Nothing @TimerEvent
      it "Timer in Media" $
        decodeClass (mkStoredEvent Played) `shouldBe` Nothing @TimerEvent

-- Domain types
data TimerEvent
  = Started
  | Ended
  deriving stock (Eq, Show, Typeable)

data MediaEvent
  = Played
  | Stopped
  deriving stock (Eq, Show, Typeable)

data WithMeta a = WithMeta
  { time :: String,
    value :: a
  }
  deriving stock (Eq, Show)

-- Technical types
data StoredEvent
  = forall a. (Typeable a) => StoredEvent (TypeRep a) a

mkStoredEvent :: (Typeable a) => a -> StoredEvent
mkStoredEvent x = StoredEvent (typeOf x) x

-- Implementation
decodeInline :: forall event. (Typeable event) => StoredEvent -> Maybe event
decodeInline (StoredEvent eTypeRep ePayload) =
  decodeFinal eTypeRep ePayload <|> unwrapWithMeta
  where
    unwrapWithMeta :: Maybe event
    unwrapWithMeta =
      case eTypeRep of
        App wrapperType wrappedType
          | Just HRefl <- eqTypeRep wrapperType (typeRep @WithMeta) ->
              decodeFinal wrappedType ePayload.value
        _ -> Nothing

decodeFinal :: forall event content. (Typeable event) => TypeRep content -> content -> Maybe event
decodeFinal cTypeRep cPayload =
  case eqTypeRep cTypeRep (typeRep @event) of
    Just HRefl -> Just cPayload
    Nothing -> Nothing

class Decodable event where
  decodeClass :: StoredEvent -> Maybe event

instance {-# OVERLAPPABLE #-} (Typeable event) => Decodable event where
  decodeClass stored@(StoredEvent eTypeRep ePayload) =
    decodeFinal eTypeRep ePayload <|> (value <$> decodeClass stored)

instance (Typeable event) => Decodable (WithMeta event) where
  decodeClass (StoredEvent eTypeRep ePayload) =
    decodeFinal eTypeRep ePayload
