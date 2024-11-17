+++
title = "From custom Monad to Polysemy"
date = 2023-10-29
draft = false
path = "2023-10/polysemy-migration"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "polysemy"]
+++

Previously we have studied [a custom `Monad`](@/blog/2023-10-25_implementation-oriented-monad.md)
and how to refactor it to make it easier to work with.

For reminder, it was this one:

```haskell
class Monad m => MyAppMonad m where
  getSmtpConfig :: m SmtpConfig
  getS3Config :: m S3Config
  getRedisParams :: m RedisParams
```

And it was used as a basis for:

```haskell
fetchUser :: (MyAppMonad m) => UserName -> m (Maybe User)
storeAvatar :: (MyAppMonad m) => UserId -> ByteString -> m ()
notifySignUp :: (MyAppMonad m) => UserId -> PassCode -> m ()
```

At some point, we would like to integrate it with a broader Polysemy-based codebase.

A first move to make would be to replace each reader by Polysemy `Reader`s effects:

```haskell
type MyAppMonadEffects =
  '[ Reader SmtpConfig,
     Reader S3Config,
     Reader RedisParams
   ]
```

Then, to avoid any change in the codebase, we have to drop in `MyAppMonad`
with a fixed `EffectsRow`:

```haskell
type MyAppMonad m = m ~ Sem MyAppMonadEffects
```

And then our getters:

```haskell
getSmtpConfig :: (Member (Reader SmtpConfig) r) => Sem r SmtpConfig
getSmtpConfig = ask

getS3Config :: (Member (Reader S3Config) r) => Sem r S3Config
getS3Config = ask

getRedisParams :: (Member (Reader RedisParams) r) => Sem r RedisParams
getRedisParams = ask
```

It's not ideal as effects being fixed dealing with this code (calling/being called by)
would require `raise` each time effects differ.

But that's a first step, then we can progressively substitute consumers with
`Sem r`/`Members` and create individual effects/interpreters.
