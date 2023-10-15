+++
title = "Haskell Type Reflection Anti-pattern"
date = 2023-10-15
draft = false
path = "2023-10/haskell-type-reflection-anti-pattern"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design"]
+++

Few days ago I was [mob programming](https://en.wikipedia.org/wiki/Team_programming)
with my teammates, we had to write some tests for an [Event Sourcing Projection](https://www.eventstore.com/event-sourcing#Projections)
which use legacy events to produce new events.

Our tests are running against an in-memory event store which represents events
as follows:

```haskell
import Type.Reflection

data StoredEvent
  = forall a. (Typeable a) => StoredEvent (TypeRep a) a

mkStoredEvent :: (Typeable a) => a -> StoredEvent
mkStoredEvent x = StoredEvent (typeOf x) x
```

our business events have this form:

```haskell
data TimerEvent
  = Started
  | Ended
  deriving stock (Eq, Show, Typeable)

data MediaEvent
  = Played
  | Stopped
  deriving stock (Eq, Show, Typeable)
```

in addition of that, we have dedicated types to enrich them:

```haskell
data WithMeta a = WithMeta
  { time :: String,
    value :: a
  }
  deriving stock (Eq, Show)
```

The goal of the test was to check that, for some legacy events, new events are
correct.

This issue is that our event store does not give type-level information on the
event type, which means we need a way to decode them.

Another concern regarding `WithMeta`, which should be removed when not required.

It should behave as follows:

```haskell
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
```

By default, Haskell does not let you access to type information at runtime,
we have to rely on [`Typeable`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Type-Reflection.html#t:Typeable).

The base function is define as follows:

```haskell
decodeFinal :: forall event content. (Typeable event) => TypeRep content -> content -> Maybe event
decodeFinal cTypeRep cPayload =
  case eqTypeRep cTypeRep (typeRep @event) of
    Just HRefl -> Just cPayload
    Nothing -> Nothing
```

The magic comes from `eqTypeRep` which compares `TypeRep a` and  `TypeRep b`,
and if they match emits `Just HRefl` which, once match, implies `a ~ b`.

Then we do something I find bad:

```haskell
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
```

It works as follows: try to directly match types, or unwrap `WithMeta` and try
again (regardless `event` is `WithMeta a` or not).

So, not only you might do extra work, but you'll be forced to add some code here
for each new wrapping types.

An alternative approach would be to define a type class:

```haskell
class Decodable event where
  decodeClass :: StoredEvent -> Maybe event
```

and finally few instances:

```haskell
instance {-# OVERLAPPABLE #-} (Typeable event) => Decodable event where
  decodeClass stored@(StoredEvent eTypeRep ePayload) =
    decodeFinal eTypeRep ePayload <|> (value <$> decodeClass stored)

instance (Typeable event) => Decodable (WithMeta event) where
  decodeClass (StoredEvent eTypeRep ePayload) =
    decodeFinal eTypeRep ePayload
```

The usage of `OVERLAPPABLE` is not great, but at least it simplifies the code and
allows extensibility.
