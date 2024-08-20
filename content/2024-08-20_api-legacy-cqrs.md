+++
title = "Haskell Legacy: CQRS"
date = 2024-08-20
draft = false
path = "2024-08/api-legacy-cqrs"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

In the [last log](@/2024-08-13_api-legacy-event-first.md) we have refactored
the code, so projections are built on top of previous effects:

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

The thing is, there are many indirections, instead, I would inline them:

```haskell
apiEventPersistentProjection ::
  forall m r.
  (Members '[Embed (ReaderT SqlBackend m)] r, MonadIO m) =>
  StreamId ->
  TrainEvent ->
  Sem r ()
apiEventPersistentProjection (StreamId streamId) =
  \case
    event@(TrainCreated {}) ->
      embed $
        insert_
          Train
            { trainTrainId = trainId,
              trainDepartureDate = event.departureDate,
              trainDepartureStation = event.departureStation,
              trainArrivalStation = event.arrivalStation
            }
    event@(BookingCreated {}) ->
      embed $
        insert_
          Booking
            { bookingBookingId = BookingId' $ fromIntegral event.id,
              bookingTrainId = TrainKey trainId,
              bookingTravelerName = event.travelerName
            }
    event@(BookingWithdrawn {}) ->
      embed $ delete $ from $ \b -> where_ (b ^. BookingId ==. val (BookingKey $ BookingId' $ fromIntegral event.id))
  where
    trainId = TrainId' streamId
```

Everything related to projections is located at the same place.

Well, everything, not quite, as a reminder, we have this interpreter which has
a view model based on a projection and a projection effect:

```haskell
interceptTrainEffectEvents ::
  forall r.
  (Members '[EventStore TrainEvent, Embed IO, Error InternalApiError, TrainProjectionEffect] r) =>
  InterpreterFor TrainEffect r
interceptTrainEffectEvents =
  interpret $
    \case
      -- ...
      TrainFetch trainId ->
        trainProjectionFetch trainId
```

This is why we can introduce [command query responsibility segregation (CQRS)](https://martinfowler.com/bliki/CQRS.html).

In brief, in means that there are two distinct paths: write (events) and
read (persistent-based for the moment).

The first step is to split effects:

```haskell
data TrainManagementEffect (m :: Type -> Type) (a :: Type) where
  TrainCreate :: DepartureDate -> DepartureStation -> ArrivalStation -> TrainManagementEffect m TrainId'

makeSem ''TrainManagementEffect

data TrainViewEffect (m :: Type -> Type) (a :: Type) where
  TrainFetch :: TrainId' -> TrainViewEffect m DisplayedTrain

makeSem ''TrainViewEffect
```

And finally interpreters:

```haskell
interpretTrainViewEffectPersistent ::
  forall m r.
  (Members '[Embed (ReaderT SqlBackend m)] r, MonadIO m) =>
  InterpreterFor TrainViewEffect r
interpretTrainViewEffectPersistent =
  interpret $
    \case
      TrainFetch trainId -> do
        Entity _ train <- embed $ getBy404 $ TrainPrimaryKey trainId
        bookings <- embed $ select $ from $ \b -> where_ (b ^. BookingTrainId ==. val (TrainKey trainId)) $> b
        return $
          DisplayedTrain
            { departureDate = train.trainDepartureDate,
              departureStation = train.trainDepartureStation,
              arrivalStation = train.trainArrivalStation,
              travelers =
                map
                  ( \entity ->
                      TravelerRef
                        { bookingId = entity.entityVal.bookingBookingId,
                          travelerName = entity.entityVal.bookingTravelerName
                        }
                  )
                  bookings
            }

interceptTrainManagementEffectEvents ::
  forall r.
  (Members '[EventStore TrainEvent, Embed IO, Error InternalApiError, TrainViewEffect] r) =>
  InterpreterFor TrainManagementEffect r
interceptTrainManagementEffectEvents =
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
```
