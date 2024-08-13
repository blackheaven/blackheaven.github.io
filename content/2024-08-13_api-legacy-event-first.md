+++
title = "Haskell Legacy: Going event-first"
date = 2024-08-13
draft = false
path = "2024-08/api-legacy-event-first"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

Previously, we have [reversed writes](@/2024-08-06_api-legacy-reversing-writes.md),
which means that we write events, then we write the legacy tables.

As a reminder, we have some code looking like that:

```haskell
interceptTrainEffectEvents ::
  forall r.
  (Members '[EventStore TrainEvent, Embed IO, Error InternalApiError, TrainProjectionEffect] r) =>
  InterpreterFor TrainEffect r
interceptTrainEffectEvents =
  interpret $
    \case
      TrainCreate
        departureDate'@(DepartureDate departureDate)
        departureStation'@(DepartureStation departureStation)
        arrivalStation'@(ArrivalStation arrivalStation) -> do
          newTrainId <- embed $ TrainId' <$> randomRIO (1000000, 9999999)
          eventStored <-
            storeEvent
              (StreamId newTrainId.unTrainId')
              (EventNumber 0)
              TrainCreated
                { departureDate = departureDate,
                  departureStation = departureStation,
                  arrivalStation = arrivalStation
                }
          case eventStored of
            Left e -> throw $ EventStoreIAE e
            Right x -> do
              trainProjectionCreate newTrainId departureDate' departureStation' arrivalStation'
              return newTrainId
      TrainFetch trainId ->
        trainProjectionFetch trainId
```

This was a first step, however it's temporal coupling, one of the worst type of
coupling.

It means that we have established _correlation_ (things happen together), not
_causation_ (things happen because they are linked by the relationships).

I'll rehash just to make the point: if tests are green, it's merely coincidental.

Your code will break (or force to change) if you:

* Add/change a view (model)/query/projection
* Reorder events persistence
* Add event persistence elsewhere

Instead of calling legacy effects as backup, we can create an interceptor in
charge of it:

```haskell
apiEventProjection ::
  (Members '[TrainProjectionEffect, BookingProjectionEffect] r) =>
  StreamId ->
  TrainEvent ->
  Sem r ()
apiEventProjection (StreamId streamId) =
  \case
    event@(TrainCreated {}) ->
      trainProjectionCreate
        trainId
        (DepartureDate event.departureDate)
        (DepartureStation event.departureStation)
        (ArrivalStation event.arrivalStation)
    event@(BookingCreated {}) ->
      bookingProjectionCreate trainId (BookingId' $ fromIntegral event.id) event.travelerName
    event@(BookingWithdrawn {}) ->
      bookingProjectionDelete (BookingId' $ fromIntegral event.id)
  where
    trainId = TrainId' streamId
```

For each event, we will perform an action to "synchronize" a stateful value.

It aims to be used with this interceptor:

```haskell
interceptEventStoreWith ::
  forall event a r.
  (StreamId -> event -> Sem r ()) ->
  Sem (EventStore event ': r) a ->
  Sem (EventStore event ': r) a
interceptEventStoreWith f =
  intercept $
    \case
      StoreEvent streamId eventNumber event -> do
        result <- storeEvent streamId eventNumber event
        forM_ result $ \() ->
          raise $ f streamId event
        return result
      FetchEvents streamId ->
        fetchEvents streamId
```

It works as follows:

* Fallbacks on the real `EventStore` effect to persist the event
* If the persistence succeeded, run the callback to perform the projection
* Return the result

Note: it is a really simple implementation, in the previous real world
implementations I have done, I have relied on diffs (i.e. pushing multiples
events and making diffs on local state, so it can be persisted independently).

We can drop the calls from our interpreter:

```haskell
interceptTrainEffectEvents ::
  forall r.
  (Members '[EventStore TrainEvent, Embed IO, Error InternalApiError, TrainProjectionEffect] r) =>
  InterpreterFor TrainEffect r
interceptTrainEffectEvents =
  interpret $
    \case
      TrainCreate
        (DepartureDate departureDate)
        (DepartureStation departureStation)
        (ArrivalStation arrivalStation) -> do
          newTrainId <- embed $ TrainId' <$> randomRIO (1000000, 9999999)
          eventStored <-
            storeEvent
              (StreamId newTrainId.unTrainId')
              (EventNumber 0)
              TrainCreated
                { departureDate = departureDate,
                  departureStation = departureStation,
                  arrivalStation = arrivalStation
                }
          case eventStored of
            Left e -> throw $ EventStoreIAE e
            Right x -> return newTrainId
      TrainFetch trainId ->
        trainProjectionFetch trainId
```

and add it to our interpreters stacks:

```haskell
runPersistent :: ConnectionPool -> EffectsRunner a
runPersistent pool =
  liftIO
    . join
    . fmap (either (throwIO . iaeToYesod) return)
    . runM
    . runError
    . runEmbedded (flip runSqlPool pool)
    . interpretBookingEffectPersistent
    . interpretTrainProjectionEffectPersistent
    . interpretEventStorePersistent
    . interceptEventStoreWith apiEventProjection
    . interpretBookingEffectEvents
    . interceptTrainEffectEvents
```

Doing so allows us to add/change our views without impacting our main, event-sourced interpreters.
