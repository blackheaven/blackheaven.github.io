+++
title = "Haskell Legacy: Testing"
date = 2024-07-16
draft = false
path = "2024-07/api-legacy-testing"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "design", "legacy"]
+++

In [the previous log](@/blog/2024-07-09_api-legacy-introduction.md), we have seen a
legacy API to create trains and book/withdraw tickets.

Whenever I land in a new codebase I have to work on, I try to add some tests
in order to stabilize the current behavior.
So, if I break tests, I can make the conscious decision to change or keep
the behavior.

The thing is, we have a code which is mainly dealing with `IO`.

For reference, the API is launched like this:

```haskell
main :: IO ()
main =
  runStderrLoggingT $
    withSqlitePool "trainsMaster.db" openConnectionCount $ \pool -> liftIO $ do
      runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
      putStrLn "Listening on port 3000"
      warp 3000 $ TrainMasterAPI pool
```

Hopefully Yesod comes with [testing capabilities](https://hackage.haskell.org/package/yesod-test).

Similarly, we have to wrap our [test cases](https://hackage.haskell.org/package/hspec-2.11.9/docs/Test-Hspec.html#t:Spec)
to provide an initialized `site` (which is more or less a Yesod app):

```haskell
runStderrLoggingT $
  withSqlitePool ":memory:" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
      runMigration migrateAll
    hspec $
      yesodSpec (TrainMasterAPI pool) spec
```

SQLite is a great embedded SQL database, it usually work on a file, but
`:memory:` create a purely in-memory database.

We can come up with two kind of test cases.

The nominal scenario:

* Creating a Train
* Make two bookings
* Withdrawing one
* Displaying the train

```haskell
yit "Nominal case: creating a train, adding/withdraw reservation and looking up" $ do
  postJson CreateTrainR CreateTrainRequest {departureDate = "2024-06-01", departureStation = "Lyon", arrivalStation = "Zurich"}
  statusIs 200
  trainCreation <- requireJSONResponse @(CreatedResponse (Key Train))

  liftIO $ print trainCreation.id
  postJson (CreateBookingR trainCreation.id) CreateBookingRequest {travelerName = "Alice"}
  statusIs 200
  traveler0Creation <- requireJSONResponse @(CreatedResponse (Key Booking))

  postJson (CreateBookingR trainCreation.id) CreateBookingRequest {travelerName = "Bob"}
  statusIs 200
  traveler1Creation <- requireJSONResponse @(CreatedResponse (Key Booking))

  deleteJson $ ManageBookingR traveler0Creation.id
  statusIs 200

  getJson $ DisplayTrainR trainCreation.id
  statusIs 200
  trainResponse <- requireJSONResponse @DisplayTrainResponse
  assertEq
    "Displayed train is valid"
    trainResponse
    DisplayTrainResponse
      { departureDate = "2024-06-01",
        departureStation = "Lyon",
        arrivalStation = "Zurich",
        travelers =
          [ TravelerRef
              { bookingId = traveler1Creation.id,
                travelerName = "Bob"
              }
          ]
      }
```

And edge cases, when you deal with unknown trains.

```haskell
yit "Edge case: booking an unknow train should fail" $ do
  postJson (CreateBookingR $ TrainKey $ SqlBackendKey 42) CreateBookingRequest {travelerName = "Alice"}
  statusIs 500
yit "Edge case: showing an unknow train should fail" $ do
  getJson $ DisplayTrainR $ TrainKey $ SqlBackendKey 42
  statusIs 500
```

Those one will change as error codes are neither explicit nor valid from an
HTTP point of view (the semantic is invalid).

Note: If you are curious, you'll notice `getJson`/`postJson`/`deleteJson` are
not defined in `Yesod.Test`.

I think Yesod did not put RESTFul APIs (JSON-based ones at least) first, I had
to come up with few helpers:

```haskell
postJson :: (ToJSON body, Yesod site) => Route site -> body -> YesodExample site ()
postJson route body =
  request $ do
    setRequestBody (encode body)
    addRequestHeader ("Accept", "application/json")
    addRequestHeader ("Content-Type", "application/json")
    setUrl route
    setMethod "POST"

getJson :: (Yesod site) => Route site -> YesodExample site ()
getJson = noBodyJson "GET"

deleteJson :: (Yesod site) => Route site -> YesodExample site ()
deleteJson = noBodyJson "DELETE"

noBodyJson :: (Yesod site) => Method -> Route site -> YesodExample site ()
noBodyJson verb route =
  request $ do
    addRequestHeader ("Accept", "application/json")
    addRequestHeader ("Content-Type", "application/json")
    setUrl route
    setMethod verb
```
