+++
title = "Extreme branchless: containers"
date = 2024-10-29
draft = false
path = "2024-10/extreme-branchless-1"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Sometime ago, I have written about a [branchless](@/2024-03-13_fizzbuzz.md)
(i.e without explicit control-flow) solution to _fizzbuzz_.

While control-flow is less damaging in functional programming (it's type-checked
and completness-checked), I was challenged to go further.

As a reminder, there is still control-flow under the hood:

```haskell
instance Semigroup a => Semigroup (Maybe a) where
  Just x <> Just y = Just $ x <> y
  Just x <> Nothing = Just x
  Nothing <> Just x = Just x
  Nothing <> Nothing = Nothing
```

Not to mention functions on list.

I had an idea at the time, but I have failed to implement, it'll be the topic
of this log and the next.

In pure lambda calculus, there are no, data structures, only functions, that's
why we have to come-up with constructions like [Church Encoding](https://en.wikipedia.org/wiki/Church_encoding).

Let's start with `Maybe` which is a simple data type representing a value
possibly missing:

```haskell
data Maybe a = Nothing | Just a
```

It comes with a function `maybe` with which you can do anything:

```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
maybe def f =
  \case
    Nothing -> def
    Just x -> f x
```

It works as follows:

* The value is missing (`Nothing`), default value is picked
* The value is present (`Just`), then we apply the function on it

In order to use pure lambda calculus, we are limited to pure functions, let's
express `maybe` in those terms:

```haskell
newtype Maybe' a = Maybe' (forall b. b -> (a -> b) -> b)
```

Note: I have wrapped it to make `instance`s definition easier

`Maybe'` is a wrapper around `maybe` type (more or less), let's see `Just`/`Nothing`
are expressed:

```haskell
nothing :: Maybe' a
nothing = Maybe' $ \def _ -> def

just :: a -> Maybe' a
just x = Maybe' $ \_ f -> f x
```

Again: `nothing` take the default value, `value` is given on the transformation function.

Then we can rewrite `maybe`:

```haskell
maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' def f (Maybe' maybeValue) = maybeValue def f
```

And it's branchless, branching being delegated to `Maybe'` creation.

Then we can rewrite the two functions we need in our code:

```haskell
fromMaybe' :: a -> Maybe' a -> a
fromMaybe' def maybeValue = maybe' def id maybeValue

instance Semigroup a => Semigroup (Maybe' a) where
  x <> y =
    maybe' y (\xValue -> maybe' (just xValue) (\yValue -> just (xValue <> yValue)) y) x
```

The other place where have underneath pattern-matching is around lists.

In Haskell they are more or less implemented as such:

```haskell
data List a = Cons a (List a) | Nill
-- data [] a = a : [] a | []
```

Moreover, we use it as streams (never-ending, thanks to laziness):

```haskell
fizzs :: [Maybe String]
fizzs = cycle [Nothing, Nothing, Just "Fizz"]
```

We could define it in pure functions as:

```haskell
data Stream a = Stream (forall b. (a -> Stream a -> b) -> b)
```

We have a function which takes an element and a `Stream`, giving a transformed value.

Let's add some sugar:

```haskell
infixr 4 .:

(.:) :: a -> Stream a -> Stream a
x .: xs = Stream $ \g -> g x xs
```

Note: with algebraic data types we could write:

```haskell
data Stream a = a .: Stream a
```

Then we could rewrite our stream functions:

```haskell
numbers :: Stream String
numbers = show <$> it 1
  where it n = n .: it (n + 1)

fizzs :: Stream (Maybe' String)
fizzs = nothing .: nothing .: just "Fizz" .: fizzs

buzzs :: Stream (Maybe' String)
buzzs = nothing .: nothing .: nothing .: nothing .: just "Buzz" .: buzzs
```

Note how each binding ("function"/"variable") references itself.

It works (don't freeze) because laziness delays evaluation until force (e.g.
usually in `IO`).

We could then define few functions:

```haskell
instance Functor Stream where
  fmap f (Stream s) = Stream $ \g -> g (s $ \x _ -> f x) (s $ \_ xs -> fmap f xs)

headStream :: Stream a  -> a
headStream (Stream f) = f $ \x _ -> x

tailStream :: Stream a  -> Stream a
tailStream (Stream f) = f $ \_ xs -> xs
```

Quite straightforward:

* `headStream` takes the element parameter
* `tailStream` takes the `Stream` parameter

Then we can define `zipWith` which takes two `Stream`s element by element:

```haskell
zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream f (Stream s) (Stream t) =
  Stream $ \g -> g (f (s $ \x _ -> x) (t $ \x _ -> x)) (zipWithStream f (s $ \_ xs -> xs) (t $ \_ xs -> xs))
```

Taking the function, we apply the functions on `head`s then recursively on `tail`s.

Finally, the `(!!)`, which is a `at`:

```haskell
atStream :: Int -> Stream a  -> a
atStream n s
  | n <= 0 = headStream s
  | otherwise = n - 1 `atStream` tailStream s
-- Alternatively it could be written as:
-- atStream n s = headStream $ iterate tailStream s !! n
-- or:
-- atStream n s = (headStream <$> iterate tailStream s) !! n
-- That's why developments cannot be consitents as they results of thousands of choices

infixr 4 `atStream'`

fizzbuzz :: Int -> String'
fizzbuzz n = n - 1 `atStream` fizzbuzzStream
```

So far so good, everything works, branchless (well, nearly, let's see what's
missing in the next log).
