+++
title = "Haskell Legacy: Reversing writes"
date = 2024-08-06
draft = false
path = "2024-08/api-legacy-reversing-writes"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

Previously, we have [introduced events](@/blog/2024-07-30_api-legacy-events.md),
however they are the second write, meaning that they are not actually used,
they are "passive", even if we fail to write them, we let the execution continue.

A first step is to reverse writes, so we ensure that events are written.

As a reminder we have interceptors calling the `persistent` interpreters:

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

In order to reverse writes we should:

* Make these interceptors interpreters (so they consume the effects), which implies
  * generate the unique `id` in them
  * have a dedicated`id` type
  * change the legacy interpreters, so the take the `id`, which implies
    * change the schema so `id` is not auto incremented

Let's change the schema first:

```haskell
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Train
    trainId TrainId'
    departureDate T.Text
    departureStation T.Text
    arrivalStation T.Text
    deriving Show
    Primary trainId
|]
```

The last line tell `persistent` that the key is provided.

Then, let's create `TrainId'` (which cannot be `Train` as `persistent` generates
one, which should be a configurable behavior IMO):

```haskell
newtype TrainId' = TrainId' {unTrainId' :: Int64}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Read, PathPiece, ToHttpApiData, FromHttpApiData, PersistField, PersistFieldSql, FromJSON, ToJSON)
```

Then we have to rework our effects, the first/external one (exposed to `Handler`s)
should take `TrainId'`:

```haskell
data TrainEffect (m :: Type -> Type) (a :: Type) where
  TrainCreate :: DepartureDate -> DepartureStation -> ArrivalStation -> TrainEffect m TrainId'
  TrainFetch :: TrainId' -> TrainEffect m DisplayedTrain

makeSem ''TrainEffect
```

Which indeed forces us to change `Handler`s and `Route`s:

```haskell
mkYesod
  "TrainMasterAPI"
  [parseRoutes|
/train CreateTrainR POST
/train/#TrainId' DisplayTrainR GET
/booking/#TrainId' CreateBookingR POST
/booking-admin/#BookingId' ManageBookingR DELETE
|]

getDisplayTrainR :: TrainId' -> HandlerFor TrainMasterAPI Value
getDisplayTrainR trainId = do
  train <- runEffect $ trainFetch trainId
  returnJson train
```

Note: that's what happen when you couple your persistence with your API.

Then we can create a second/internal effect for the legacy effect:

```haskell
data TrainProjectionEffect (m :: Type -> Type) (a :: Type) where
  TrainProjectionCreate :: TrainId' -> DepartureDate -> DepartureStation -> ArrivalStation -> TrainProjectionEffect m ()
  TrainProjectionFetch :: TrainId' -> TrainProjectionEffect m DisplayedTrain

makeSem ''TrainProjectionEffect
```

At this point, we have to adapt our legacy interpreter to take the provided `id`:

```haskell
interpretTrainProjectionEffectPersistent ::
  forall m r.
  (Members '[Embed (ReaderT SqlBackend m)] r, MonadIO m) =>
  InterpreterFor TrainProjectionEffect r
interpretTrainProjectionEffectPersistent =
  interpret $
    \case
      TrainProjectionCreate
        newTrainId
        (DepartureDate departureDate)
        (DepartureStation departureStation)
        (ArrivalStation arrivalStation) ->
          embed $
            insert_
              Train
                { trainTrainId = newTrainId,
                  trainDepartureDate = departureDate,
                  trainDepartureStation = departureStation,
                  trainArrivalStation = arrivalStation
                }
      TrainProjectionFetch trainId -> do
        -- ...
```

Then, we can rewrite our external interpreters:

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

Note: at this point, we are still using the legacy view model (`trainProjectionFetch`)

Here, we force the persistence of the events before going on

Finally, we can add our new effects to the list:

```haskell
type APIEffects =
  '[ TrainEffect,
     TrainProjectionEffect,
     BookingEffect,
     BookingProjectionEffect
     EventStore TrainEvent,
     Embed (ReaderT SqlBackend IO),
     Error InternalApiError,
     Embed IO
   ]
```

And everything works as previously.
