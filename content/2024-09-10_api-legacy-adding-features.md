+++
title = "Haskell Legacy: Adding features"
date = 2024-09-10
draft = false
path = "2024-08/api-legacy-adding-features"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy", "polysemy"]
+++

After a short break, let's go back to our [Event-sourced API](@/2024-08-27_api-legacy-pure-projection.md).

Until now, we have only changed the internals of our API.

Our goal is to eventually add a reservation system (I.e. given a train with
limited tickets, customers should be able to temporarily "lock" tickets, until
they make the payment or a timeout occurs).

To begin, we have to be able to create trains with a capacity:

```haskell
data TrainEvent
  = TrainCreated
      { departureDate :: T.Text,
        departureStation :: T.Text,
        arrivalStation :: T.Text,
        capacity :: Maybe Int
      }
  -- ...
```

Note: here, I have choosen to add an optional field to an existing event,
it's not bad _per se_, but it's done without event versioning or
[DTO](https://en.wikipedia.org/wiki/Data_transfer_object).
Moreover, it prevents us to have interesting types.
Alternatively, we could either add a new event, or rewrite the events stream.

We then have to pass it along in the API (I skip it, since it's pretty mechanical).

Then, we have to check the capacity when booking is done:

```haskell
interpretBookingEffectEvents =
  interpret $
    \case
      BookingCreate trainId travelerName -> do
        let trainStreamId = StreamId trainId.unTrainId'
        events <- fetchEvents trainStreamId
        when (null events) $
          throw NotFoundIAE

        let capacityChange =
              \case
                event@(TrainCreated {}) -> fromMaybe 1000000 event.capacity -- hopefully, no train will have 1 million traveler
                BookingCreated {} -> -1
                BookingWithdrawn {} -> 1
        when (sum (capacityChange . snd <$> events) <= 0) $
          throw TooMuchIAE

        newBookingId <- embed $ BookingId' <$> randomRIO (1000000, 9999999)
        void $
          storeEvent
            trainStreamId
            (EventNumber $ fromIntegral $ length events)
            BookingCreated {id = fromIntegral newBookingId.unBookingId', travelerName = travelerName}

        return newBookingId
```

Then we can add reservation events:

```haskell
data TrainEvent
  -- ...
  | BookingReserved {token :: T.Text}
  | BookingReservationWithdrawn {token :: T.Text}
```

We then have to add a new operation in the effect:

```haskell
newtype BookingReservationToken = BookingReservationToken {unBookingReservationToken :: T.Text}
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (FromJSON, ToJSON, PathPiece)

data BookingEffect (m :: Type -> Type) (a :: Type) where
  BookingReserve :: TrainId' -> BookingEffect m BookingReservationToken
  BookingCreate :: TrainId' -> BookingReservationToken -> T.Text -> BookingEffect m BookingId'
  BookingDelete :: TrainId' -> BookingId' -> BookingEffect m ()
```

We also have to adapt our API:

```haskell
mkYesod
  "TrainMasterAPI"
  [parseRoutes|
/train CreateTrainR POST
/train/#TrainId' DisplayTrainR GET
/booking/reserve/#TrainId' ReserveBookingR POST
/booking/book/#TrainId'/#BookingReservationToken CreateBookingR POST
/booking/unbook/#TrainId'/#BookingId' ManageBookingR DELETE
|]

-- ...

newtype BookinReserveResponse = BookinReserveResponse
  { token :: BookingReservationToken
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

postReserveBookingR :: TrainId' -> HandlerFor TrainMasterAPI (JSONResponse BookinReserveResponse)
postReserveBookingR trainId = do
  token <- runEffect $ bookingReserve trainId
  return $ JSONResponse $ BookinReserveResponse token

newtype CreateBookingRequest = CreateBookingRequest
  { travelerName :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

postCreateBookingR ::
  TrainId' ->
  BookingReservationToken ->
  HandlerFor TrainMasterAPI (JSONResponse (CreatedResponse BookingId'))
postCreateBookingR trainId token = do
  booking <- requireCheckJsonBody @_ @CreateBookingRequest
  bookingId <- runEffect $ bookingCreate trainId token booking.travelerName
  return $ JSONResponse $ CreatedResponse bookingId
```

Note: it would be a good use case for [HATEOAS](https://en.wikipedia.org/wiki/HATEOAS),
if it [had proper tooling](@/2024-09-03_autogriff-restrospective.md).

Then, we can create our new interpretation:

```haskell
\case
  BookingReserve trainId -> do
    let trainStreamId = StreamId trainId.unTrainId'
    events <- map snd <$> fetchEvents trainStreamId
    when (null events) $
      throw NotFoundIAE

    let hasSpareTicket events = sum (capacityChange <$> events) <= 0
        capacityChange =
          \case
            event@(TrainCreated {}) -> fromMaybe 1000000 event.capacity -- hopefully, no train will have 1 million traveler
            BookingCreated {} -> -1
            BookingWithdrawn {} -> 1
            BookingReserved {} -> -1
            BookingReservationWithdrawn {} -> 1
    when (hasSpareTicket events) $
      throw TooMuchIAE

    newBookingReservationToken <- embed $ T.pack . show <$> randomRIO @Int (1000000, 9999999)
    void $
      storeEvent
        trainStreamId
        (EventNumber $ fromIntegral $ length events)
        BookingReserved {token = newBookingReservationToken}

    return $ BookingReservationToken newBookingReservationToken
  -- ...
```

Finally, adapt our booking creation.

```haskell
\case
  -- ...
  BookingCreate trainId (BookingReservationToken token) travelerName -> do
    let trainStreamId = StreamId trainId.unTrainId'
    events0 <- fetchEvents trainStreamId
    events <- map snd <$> fetchEvents trainStreamId
    when (null events || BookingReserved token `notElem` events || BookingReservationWithdrawn token `elem` events) $
      throw NotFoundIAE

    newBookingId <- embed $ BookingId' <$> randomRIO (1000000, 9999999)
    void $
      storeEvent
        trainStreamId
        (EventNumber $ fromIntegral $ length events)
        BookingCreated {id = fromIntegral newBookingId.unBookingId', travelerName = travelerName}
    void $
      storeEvent
        trainStreamId
        (EventNumber $ fromIntegral $ 1 + length events)
        BookingReservationWithdrawn {token = token}

    return newBookingId
  -- ...
```

Note: we don't check again the capacity.

Lastly, we could imagine a system to regularly garbage collect old reservations.
