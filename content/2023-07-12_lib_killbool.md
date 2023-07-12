+++
title = "New library: kill-bool"
date = 2023-07-12
draft = false
path = "2023-07/lib-kill-bool"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "library"]
+++

A while ago, I was working on some cache-based code, I had a primitive function:

```haskell
fetchCached :: Member (Cached key value) r => key -> Sem r (Bool, value)
```

It was intended to be used as follows:

```haskell
ssoSignin :: Members SomeEffects r => SsoParams -> Sem r (User, Maybe Welcome)
ssoSignin p = do
  userInfo <- fetchSsoInfo p
  (wasCached, user) <- fetchCached userInfo.id
  let sayWelcomeIfNew =
        if wasCached
          then return Nothing
          else Just <$> mkWelcome user

  (,) user <$> sayWelcomeIfNew
```

Actually, this design is kind of flawed, `Bool` being not explicit enough to know from the type signature whether `True` mean it was cached or it is new.

Which quickly led to this mistake:

```haskell
something :: Members SomeEffects r => Args -> Sem r ()
something p = do
  -- ...
  (wasCached, y) <- fetchCached x
  unless wasCached $
    doSomethingAtCreation y
```

A solution would be to create a dedicated type, but instead, I made a simple library, [_kill-bool_](https://hackage.haskell.org/package/kill-bool), to make sense of `Bool`s without the burden of defining one-time types..

```haskell
fetchCached :: Member (Cached key value) r => key -> Sem r (TBool "cached" "created", value)
```

(`TBool` for _Typed_-`Bool`)

Which helps us to write less ambiguous code:

```haskell
ssoSignin :: Members SomeEffects r => SsoParams -> Sem r (User, Maybe Welcome)
ssoSignin p = do
  userInfo <- fetchSsoInfo p
  (wasCached, user) <- fetchCached userInfo.id
  let sayWelcomeIfNew =
        if is (Proxy @"cached") wasCached
          then return Nothing
          else Just <$> mkWelcome user

  (,) user <$> sayWelcomeIfNew
```

Note: it was named after Quentin Tarantino's [Kill Bill movies](https://en.wikipedia.org/wiki/Kill_Bill)
