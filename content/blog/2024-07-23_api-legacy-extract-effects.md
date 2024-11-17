+++
title = "Haskell Legacy: Extracting business logic and introducing effects"
date = 2024-07-23
draft = false
path = "2024-07/api-legacy-extract-effects"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

One of the issue of [our API](@/blog/2024-07-09_api-legacy-introduction.md) is the
direct implementation of the "business logic" (which is quite thin and coupled
to the persistence) directly in `Handler`s.

It's quite problematic as, not only our [tests](@/blog/2024-07-16_api-legacy-testing.md)
are slow, but having everything (framework plumbing, business logic, persistence)
at the same place is an issue:

* changing one thing might silently change another, e.g. changing persistence might affect business logic
* coupling degrades readability
* coupling will make maintenance and evolution costly

The first easy step is to go from all-at-once `Handler`s:

```haskell
getDisplayTrainR :: TrainId -> HandlerFor TrainMasterAPI Value
getDisplayTrainR trainId = do
  train <- runDB $ get404 trainId
  bookings <- runDB $ selectList [BookingTrainId ==. trainId] []
  returnJson $
    DisplayTrainResponse
      { departureDate = train.trainDepartureDate,
        departureStation = train.trainDepartureStation,
        arrivalStation = train.trainArrivalStation,
        travelers =
          map
            ( \entity ->
                TravelerRef
                  { bookingId = entity.entityKey,
                    travelerName = entity.entityVal.bookingTravelerName
                  }
            )
            bookings
      }
```

To an `Handler` dedicated to plumbing:

```haskell
postCreateBookingR :: TrainId -> HandlerFor TrainMasterAPI Value
postCreateBookingR trainId = do
  booking <- requireCheckJsonBody @_ @CreateBookingRequest
  bookingId <- bookingCreate trainId booking.travelerName
  returnJson $ CreatedResponse bookingId
```

And a function dedicated to logic:

```haskell
bookingCreate :: TrainId -> T.Text -> HandlerFor TrainMasterAPI BookingId
bookingCreate trainId travelerName =
  runDB $
    insert400
      Booking
        { bookingTrainId = trainId,
          bookingTravelerName = travelerName
        }
```

That's a start, the next step is to get out of `HandlerFor`, so we can gain in
flexibility regarding the underlying implementation.

We can introduce an effect having a similar type definition:

```haskell
data BookingEffect (m :: Type -> Type) (a :: Type) where
  BookingCreate :: TrainId -> T.Text -> BookingEffect m BookingId
  BookingDelete :: BookingId -> BookingEffect m ()

makeSem ''BookingEffect
```

Now we have an integration issue: working with [`polysemy`](https://hackage.haskell.org/package/polysemy)
(the effects system library), will give you `Sem r a`, while Yesod expects
`HandlerFor TrainMasterAPI a`.

In order to simplify that, let's define some types, one for the effects we will
use, another one for effect interpretation:

```haskell
type APIEffects = '[TrainEffect, BookingEffect, Embed (ReaderT SqlBackend IO), Embed IO]

type EffectsRunner a = Sem APIEffects a -> HandlerFor TrainMasterAPI a
```

The most efficient way to pass it around (and to make it parameterizable) is
to store it directly in the API configuration type:

```haskell
data TrainMasterAPI = TrainMasterAPI (forall a. EffectsRunner a)
```

So, eventually, we can come up with a helper:

```haskell
runEffect :: EffectsRunner a
runEffect sem = do
  TrainMasterAPI f <- getYesod
  f sem
```

We can then substitute it in the `Handler`s:

```haskell
postCreateBookingR :: TrainId -> HandlerFor TrainMasterAPI Value
postCreateBookingR trainId = do
  booking <- requireCheckJsonBody @_ @CreateBookingRequest
  bookingId <- runEffect $ bookingCreate trainId booking.travelerName
  returnJson $ CreatedResponse bookingId

deleteManageBookingR :: BookingId -> HandlerFor TrainMasterAPI Value
deleteManageBookingR bookingId = do
  runEffect $ bookingDelete bookingId
  returnJson ()
```

Until now, I have left something aside: interpreters.

One of the benefit of effects systems is to be able to decouple interface/contract
(the effect definition), from the implementations (interpreters).

Actually, interpreters are just the old logic we had extracted earlier:

```haskell
interpretBookingEffectPersistent ::
  forall m r.
  (Members '[Embed (ReaderT SqlBackend m)] r, MonadIO m) =>
  InterpreterFor BookingEffect r
interpretBookingEffectPersistent =
  interpret $
    \case
      BookingCreate trainId travelerName ->
        embed @(ReaderT SqlBackend m) $
          insert400
            Booking
              { bookingTrainId = trainId,
                bookingTravelerName = travelerName
              }
      BookingDelete bookingId ->
        embed @(ReaderT SqlBackend m) $ delete bookingId
```

To be complete, let's write a composed interpreter for the whole API:

```haskell
runPersistent :: ConnectionPool -> EffectsRunner a
runPersistent pool =
  liftIO
    . runM
    . runEmbedded (flip runSqlPool pool)
    . interpretBookingEffectPersistent
    . interpretTrainEffectPersistent
```

So we can use it in the API launch:

```haskell
main :: IO ()
main =
  runStderrLoggingT $
    withSqlitePool "repl.db" openConnectionCount $ \pool -> liftIO $ do
      runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
      putStrLn "Listening on port 3000"
      warp 3000 $ TrainMasterAPI $ runPersistent pool
```
