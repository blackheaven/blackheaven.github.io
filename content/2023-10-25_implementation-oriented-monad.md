+++
title = "Implementation-oriented Monad"
date = 2023-10-25
draft = false
path = "2023-10/implementation-oriented-monad"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design"]
+++

I am currently migrating some hand-crafted `Monad` looking like this:

```haskell
class MyAppMonad m where
  getSmtpConfig :: m SmtpConfig
  getS3Config :: m S3Config
  getRedisParams :: m RedisParams
```

And some helpers used a bit everywhere:

```haskell
fetchUser :: (MyAppMonad m) => UserName -> m (Maybe User)
storeAvatar :: (MyAppMonad m) => UserId -> ByteString -> m ()
notifySignUp :: (MyAppMonad m) => UserId -> PassCode -> m ()
```

I call these _implementation-oriented_ `Monad`s, as they restrict implementation
to mostly one implementation:

```haskell
newtype MyAppM a = MyAppM {runMyAppM :: ReaderT AppContext IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)

data AppContext = AppContext
  { acSmtpConfig :: SmtpConfig,
    acRedisParams :: RedisParams,
    acS3Config :: S3Config
  }

instance MyAppMonad MyAppM where
  getSmtpConfig = MyAppM $ asks acSmtpConfig
  getS3Config = MyAppM $ asks acS3Config
  getRedisParams = MyAppM $ asks acRedisParams
```

it suffers from the following drawbacks:

* Static behaviors (only backends' parameters can be changed, not behaviors)
* High coupling (every piece of code is coupled to the whole `Monad`, even if they are only using one member, or for one purpose)
* Types opacity

The most basic refactoring would be to integrate helpers into the `type class`:

```haskell
class MyAppMonad m where
  fetchUser :: UserName -> m (Maybe User)
  storeAvatar :: UserId -> ByteString -> m ()
  notifySignUp :: UserId -> PassCode -> m ()
```

We still have the type opacity, but at least, we are free to have a completely
different implementation, such as a stub:

```haskell
newtype NoopAppM a = NoopAppM {runNoopAppM :: IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MyAppMonad' NoopAppM where
  fetchUser' _ = NoopAppM $ return Nothing
  storeAvatar' _ _ = NoopAppM $ return ()
  notifySignUp' _ _ = NoopAppM $ return ()
```
