+++
title = "Abaks: Interface Adapters - API"
date = 2023-06-25
draft = false
path = "2023-06/abaks-interface-adapters-api"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

After the [Use Cases](@/blog/2023-06-18-abaks-use-cases.md) there are the _Interface Adapters_, namely the HTTP (REST) API.

We'll use [_servant_](https://docs.servant.dev/en/stable/) to build a REST API.

Let's start by setting the layout:

```haskell
type API = NamedRoutes API'

data API' r = API
  { createPeriodAPI :: r :- CreatePeriodAPI,
    createEntryAPI :: r :- CreateEntryAPI,
    changeAmountEntryAPI :: r :- ChangeAmountEntryAPI,
    validateEntryAPI :: r :- ValidateEntryAPI,
    markInConflictEntryAPI :: r :- MarkInConflictEntryAPI,
    commentEntryAPI :: r :- CommentEntryAPI,
    deleteEntryAPI :: r :- DeleteEntryAPI
  }
  deriving stock (Generic)
```

We have declare all the endpoints we can define such as:

```haskell
type CreatePeriodAPI =
  Summary "Create a Period"
    :> OperationId "createPeriod"
    :> ReqBody '[JSON] CreatePeriodRequest
    :> Post '[JSON] CreatePeriodResponse

data CreatePeriodRequest = CreatePeriodRequest
  { name :: Text,
    from :: Day,
    to :: Day,
    initialBalance :: AmountA
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CreatePeriodResponse = CreatePeriodResponse
  { periodId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

createPeriodHandler ::
  Members ApiEffects r =>
  CreatePeriodRequest ->
  Sem r CreatePeriodResponse
createPeriodHandler req =
  genericUseCaseHandler (CreatePeriodResponse . (.getPeriodId.getAggregateId)) $
    UC.createPeriod req.name req.from req.to $
      toAmount req.initialBalance
```

It's actually a naive implementation, especially the error handling:

```haskell
genericUseCaseHandler ::
  Members ApiEffects r =>
  (a -> b) ->
  Sem r (Either Text a) ->
  Sem r b
genericUseCaseHandler onSuccess f = do
  r <- f
  case r of
    Left e -> throw err500 {errBody = TLE.encodeUtf8 $ TL.fromStrict e}
    Right s -> return $ onSuccess s
```

Note that `A`-suffixed types are [`DTO`s](https://en.wikipedia.org/wiki/Data_transfer_object), used to stabilize the API.

```haskell
newtype AmountA = AmountA {cents :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

toAmount :: AmountA -> Entities.Amount
toAmount x = Entities.Amount x.cents
```
