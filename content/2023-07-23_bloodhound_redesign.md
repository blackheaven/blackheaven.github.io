+++
title = "Bloodhound redesign progress"
date = 2023-07-23
draft = false
path = "2023-07/bloodhound-redesign"

[taxonomies]
categories = ["dev"]
tags = ["haskell"]
+++

In my previous company we used to work with [ElasticSearch](https://fr.wikipedia.org/wiki/Elasticsearch),
after some times I became the maintainer of [Bloodhound](https://hackage.haskell.org/package/bloodhound).

While I planned to increase type-safety and so on and so forth, I did not work on it.

I only added a phantom type and a proper `BHResponse` as a standard return type
(instead of an "untyped" [`http-client` `Response`](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html#t:Response)).

Which gives functions like that:

```haskell
flushIndex :: MonadBH m => IndexName -> m (BHResponse ShardResult)
deleteIndex :: MonadBH m => IndexName -> m (BHResponse Acknowledged)
getDocument :: (FromJSON a, MonadBH m) => IndexName -> DocId -> m (BHResponse (EsResult a))
```

Few days ago we had a bug report stating that one of `getDocument` wasn't working when a document isn't found (aka, it throws a `404`).

The thing is, `EsResult` deals with not-found from the payload only:

```haskell
data EsResult a = EsResult
  { _index :: Text,
    _type :: Text,
    _id :: Text,
    foundResult :: Maybe (EsResultFound a)
  }
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (EsResult a) where
  parseJSON jsonVal@(Object v) = do
    found <- v .:? "found" .!= False
    fr <-
      if found
        then parseJSON jsonVal
        else return Nothing
    EsResult
      <$> v .: "_index"
      <*> v .: "_type"
      <*> v .: "_id"
      <*> pure fr
  parseJSON _ = empty
```

The issue comes from the body parsing, there are two ways to parse a `BHResponse` body: one which takes care of the status code, and the other which does not.

```haskell
parseEsResponse ::
  ( MonadThrow m,
    FromJSON body
  ) =>
  BHResponse body ->
  m (ParsedEsResponse body)
parseEsResponse response
  | isSuccess response = case eitherDecode body of
      Right a -> return (Right a)
      Left err ->
        tryParseError err
  | otherwise = tryParseError "Non-200 status code"
  where
    body = responseBody $ getResponse response
    tryParseError originalError =
      case eitherDecode body of
        Right e -> return (Left e)
        -- Failed to parse the error message.
        Left err -> explode ("Original error was: " <> originalError <> " Error parse failure was: " <> err)
    explode errorMsg = throwM $ EsProtocolException (T.pack errorMsg) body

eitherDecodeResponse ::
  FromJSON a =>
  BHResponse a ->
  Either String a
eitherDecodeResponse = eitherDecode . responseBody . getResponse
```

To structurally solve this issue, a first step is to add proper types:

```haskell
-- | 'Request' upon Elasticsearch's server.
--
-- @contextualized@ is a phantom type for the expected status-dependancy
-- @responseBody@ is a phantom type for the expected result
data BHRequest contextualized responseBody = BHRequest
  { bhRequestMethod :: NHTM.Method,
    bhRequestEndpoint :: Endpoint,
    bhRequestBody :: Maybe BL.ByteString
  }
  deriving stock (Eq, Show)
```

The second step is to merge `Request` sending and body parsing, simplifying a lot the API:

```haskell
flushIndex :: MonadBH m => IndexName -> m ShardResult
deleteIndex :: MonadBH m => IndexName -> m Acknowledged
getDocument :: (FromJSON a, MonadBH m) => IndexName -> DocId -> m (EsResult a)
```

The thing with this design is:

* It changes a lot the consumer code
* It hides a `BHResponse`, which can be useful

Currently, I'm heavily working of having a stable version of this concept, but my next step would be to expose `BHRequest`:

```haskell
flushIndex :: IndexName -> BHRequest ContextDependent ShardResult
deleteIndex :: IndexName -> BHRequest ContextDependent Acknowledged
getDocument :: FromJSON a => IndexName -> DocId -> BHRequest ContextIndependent (EsResult a)
```

Which will change consumer as follows:

```haskell
perform request
perform $ withResponse request
```
