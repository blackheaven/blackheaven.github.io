+++
title = "Haskell macros"
date = 2023-07-19
draft = false
path = "2023-07/haskell-macros"

[taxonomies]
categories = ["dev"]
tags = ["haskell"]
+++

A while ago I was looking for something in `base`, when I landed in [`Foreign.Storable`](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Foreign.Storable.html#line-160).

Which made me realized that Haskell has pure C/C++ macros, not only `#if`/`#else` to check Cabal-defined flags.

A dirty thought crossed my mind, we could define something like:

```haskell
{-# LANGUAGE CPP #-}

module C where

type Rule = Int -> Maybe String

#define RULE(name, n, text) \
(name :: Rule) = \x -> if mod x n == 0 then Just "text" else Nothing

RULE (fizz, 3, "Fizz")
RULE (buzz, 5, "Buzz")
RULE (fizzBuzz, 15, "FizzBuzz")
```

Once evaluated, it gives:

```haskell
-- ...
{-# LANGUAGE CPP #-}

module C where

type Rule = Int -> Maybe String




(fizz :: Rule) = \x -> if mod x  3 == 0 then Just " Fizz" else Nothing
(buzz :: Rule) = \x -> if mod x  5 == 0 then Just " Buzz" else Nothing
(fizzBuzz :: Rule) = \x -> if mod x  15 == 0 then Just " FizzBuzz" else Nothing
```

A bit dirty but we have what we want:

```haskell
type Rule :: Type
type Rule = Int -> Maybe String
fizz :: Rule
buzz :: Rule
fizzBuzz :: Rule
```

To go further : [the excellent Aelve Guide](https://guide.aelve.com/haskell/cpp-vww0qd72).
