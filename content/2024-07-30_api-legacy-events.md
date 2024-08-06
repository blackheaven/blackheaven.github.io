+++
title = "Haskell Legacy: Creating events"
date = 2024-07-30
draft = false
path = "2024-07/api-legacy-extract-events"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

Currently, [our API](@/2024-07-09_api-legacy-introduction.md) has [effects](@/2024-07-23_api-legacy-extract-effects.md),
it changed the way we deal with the code, but fundamentally, it does not change
the way we will meet business needs.

Conceptually, events sourced system are based on:

* Commands: intentions, actions which are tried to be performed, they optionally emit one or more events
* Events: facts, important thins which have happened, they are persistent/accumulated

Commands and Events are grouped by [aggregate](https://martinfowler.com/bliki/DDD_Aggregate.html),
although, they are multiple ways to represent Commands and Events, I have chosen
*Command handlers* (commands are modeled as regular functions which emit events)
and *event stream* (events are grouped in sequential unbounded collections).

Before diving into the design, we miss and important piece: the persistence,
it's called an event store, and it has two basic operations:

```haskell
newtype StreamId
  = StreamId Int
  deriving stock (Eq, Show)

newtype EventNumber
  = EventNumber Int
  deriving stock (Eq, Show)

data StoreEventError
  = DupliicatedEventSEE
  deriving stock (Eq, Show)

data EventStore (event :: Type) (m :: Type -> Type) (a :: Type) where
  StoreEvent :: StreamId -> EventNumber -> event -> EventStore event m (Either StoreEventError ())
  FetchEvents :: StreamId -> EventStore event m [(EventNumber, event)]

makeSem ''EventStore
```

Note: it is a basic implementation, not type-safe (events type is not saved
nor checked).

We can start designing our events, mostly based on our effects:

```haskell
data TrainEvent
  = TrainCreated
      { departureDate :: T.Text,
        departureStation :: T.Text,
        arrivalStation :: T.Text
      }
  | BookingCreated {id :: Int, travelerName :: T.Text}
  | BookingWithdrawn {id :: Int}
```

Note: Events are always expressed in the past tense

Note: we consider our effects as our *Command handlers*

Then, we have to implement these *Command handlers*, usually, in [polysemy](https://hackage.haskell.org/package/polysemy),
you have interpreters, which *interpret* and *consume* effects, but here,
at this time, we want to push a first version which registers only things
which happened (a bit like a [write-through cache](https://docs.aws.amazon.com/whitepapers/latest/database-caching-strategies-using-redis/caching-patterns.html#write-through)).

In order to acheive that, we have to create an *interceptor*, which
*interprets* effects, with *consumming* them, so we can call the "legacy"/"regular"
logic.

Note: interceptors are one thing which sets *polysemy* and some other effects systems,
it's a kind of [aspect-oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming),
the ability to change system's effects without changing it.

Let's take an example:

```haskell
interceptBookingEffectEvents ::
  forall a m r.
  (Members '[EventStore TrainEvent, Error InternalApiError] r) =>
  Sem (BookingEffect ': r) a ->
  Sem (BookingEffect ': r) a
interceptBookingEffectEvents =
  intercept $ -- 1
    \case
      BookingCreate trainId travelerName -> do
        bookingId <- bookingCreate trainId travelerName -- 2
        let BookingKey bookingId' = bookingId
        events <- fetchEvents $ trainStreamId trainId -- 3
        unless (null events) $
          void $
            storeEvent -- 4
              (trainStreamId trainId)
              (EventNumber $ length events)
              BookingCreated {id = fromIntegral bookingId', travelerName = travelerName}
        return bookingId
      BookingDelete bookingId ->
        -- ...
```

1. That's all it takes to go from an interpreter (`interpret`) to an interceptor (`intercept`)
2. We emit again the effect (so legacy interpreter is called), we'll get an id, the "thing" is done
3. We fetch events, in order to be sure the aggregate has been initialized (implicitly with `TrainCreated`)
4. We register the event (the fact a new booking has been registered)

Finally, we have to add our interceptors in our api interpreter:

```haskell
runPersistent :: ConnectionPool -> EffectsRunner a
runPersistent pool =
  -- ...
    . interpretBookingEffectPersistent -- interpret Booking (legacy)
    . interceptBookingEffectEvents -- intercept Booking (event)
    . interpretTrainEffectPersistent -- interpret Train (legacy)
    . interceptTrainEffectEvents -- intercept Train (event)
```

Tests are passing, the behavior is left unchanged, however events are only
emitted for new trains.

In the next log, we'll see:

* Some strategies to migrate old/legacy trains
* How to revert write order (making events first-class)
