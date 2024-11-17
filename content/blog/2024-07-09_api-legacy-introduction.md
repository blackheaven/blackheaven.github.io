+++
title = "Haskell Legacy: Introduction"
date = 2024-07-09
draft = false
path = "2024-07/api-legacy-introduction"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy"]
+++

A long time ago, I was asked to write on polysemy and Yesod integration, on
another hand, I was also asked on the best way to migrate from regular,
state-persisting based, legacy API, to event source system.

Note: I have never used Yesod before, so it won't be difficult for me to come
up with legacy code.

In order to do that, let's see what we have: it's a train reservation API.

We can create trains, (un-)book in them.

It's described as such:

```haskell
mkYesod
  "TrainMasterAPI"
  [parseRoutes|
/train CreateTrainR POST
/train/#TrainId DisplayTrainR GET
/booking/#TrainId CreateBookingR POST
/booking-admin/#BookingId ManageBookingR DELETE
|]
```

It's usable as simply as:

```
$ curl http://localhost:3000/train --json '{"departureDate": "2024-02-15", "departureStation": "Lyon", "arrivalStation": "Zurich"}'
{
    "id": 2
}

$ curl http://localhost:3000/booking/2 --json '{"travelerName": "Alice"}'
{
    "id": 4
}

$ curl http://localhost:3000/booking/2 --json '{"travelerName": "Bob"}'
{
    "id": 5
}

$ curl http://localhost:3000/train/2
{
    "arrivalStation": "Zurich",
    "departureDate": "2024-02-15",
    "departureStation": "Lyon",
    "travelers": [
        {
            "bookingId": 4,
            "travelerName": "Alice"
        },
        {
            "bookingId": 5,
            "travelerName": "Bob"
        }
    ]
}
```

The code is designed around a simple database scheme (based on `parsistant`):

```haskell
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Train
    departureDate T.Text
    departureStation T.Text
    arrivalStation T.Text
    deriving Show
Booking
    travelerName T.Text
    trainId TrainId
    deriving Show
|]
```

I won't paste all the code here, but `Handler`s are really tightly coupled to
the database:

```haskell
data CreateTrainRequest = CreateTrainRequest
  { departureDate :: T.Text,
    departureStation :: T.Text,
    arrivalStation :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CreatedResponse a = CreatedResponse
  { id :: a
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

postCreateTrainR :: HandlerFor TrainMasterAPI Value
postCreateTrainR = do
  train <- requireCheckJsonBody @_ @CreateTrainRequest
  trainId <-
    runDB $
      insert
        Train
          { trainDepartureDate = train.departureDate,
            trainDepartureStation = train.departureStation,
            trainArrivalStation = train.arrivalStation
          }
  returnJson $ CreatedResponse trainId
```

It cannot be simpler than that (while remaining interesting), we'll follow the
following plan to tackle it:

* Add few tests
* Extract logic from `Handler`s
* Introduce event sourcing
* Substitute event sourcing to current implementation
* Introduce effects
