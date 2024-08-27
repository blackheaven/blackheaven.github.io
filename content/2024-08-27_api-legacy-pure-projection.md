+++
title = "Haskell Legacy: Pure projection"
date = 2024-08-27
draft = false
path = "2024-08/api-legacy-pure-projection"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

[Previously](@/2024-08-20_api-legacy-cqrs.md) we have extracted our views, so
we had an interpreter based on persistent-projections:

```haskell
data TrainViewEffect (m :: Type -> Type) (a :: Type) where
  TrainFetch :: TrainId' -> TrainViewEffect m DisplayedTrain

makeSem ''TrainViewEffect

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
```

It's great on legacy systems for a smooth refactoring, however, for new projects
creating projections, queries, schema can be too much (especially if you want to
build a MVP or a proof-of-concept, or simply because you are not sure that it'll
need to be *that* fast).

Instead, you can simply load events and compute the projections:

```haskell
interpretTrainViewEffectEvents ::
  forall r.
  (Members '[EventStore TrainEvent, Embed IO, Error InternalApiError] r) =>
  InterpreterFor TrainViewEffect r
interpretTrainViewEffectEvents =
  interpret $
    \case
      TrainFetch trainId -> do
        let trainStreamId = StreamId trainId.unTrainId'
        events <- map snd <$> fetchEvents trainStreamId
        when (null events) $
          throw NotFoundIAE

        let go state =
              \case
                event@(TrainCreated {}) ->
                  DisplayedTrain
                    { departureDate = event.departureDate,
                      departureStation = event.departureStation,
                      arrivalStation = event.arrivalStation,
                      travelers = []
                    }
                event@(BookingCreated {}) ->
                  state
                    { travelers =
                        state.travelers
                          <> [ TravelerRef
                                 { bookingId = BookingId' $ fromIntegral event.id,
                                   travelerName = event.travelerName
                                 }
                             ]
                    }
                event@(BookingWithdrawn {}) ->
                  let bookingId = BookingId' $ fromIntegral event.id
                   in state
                        { travelers = filter ((/= bookingId) . (.bookingId)) state.travelers
                        }
        return $ foldl go (error "Invalid stream") events
```

Note: we I introduce it, it can be controversial, it's true that this of kind
implementations can be potentially expensive, but it's perfectly valid for not
frequently used views, or features we aim to explore.

Note 2: if this view become critical, a simple key-value store can be used as
cache, even being built incrementally, event-by-event.
