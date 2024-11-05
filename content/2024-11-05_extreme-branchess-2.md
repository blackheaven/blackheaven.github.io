+++
title = "Extreme branchless: primitives"
date = 2024-11-05
draft = false
path = "2024-11/extreme-branchless-2"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Previously, we have tackled `Maybe`/`Stream` [branchless](@/2024-10-29_extreme-branchess-1.md).

But we still have many Algebraic data-types under the hood, such as... `String`s,
which are defined as:

```haskell
type String = [Char]
```

A plain old list of `Char`, let's define it.

A `List` is a `Stream` with a end (one more constructor):

```haskell
newtype List a = List (forall b. b -> (a -> List a -> b) -> b)
```

We could define few utils:

```haskell
toList :: [a] -> List a
toList =
  \case
    [] -> List $ \h _ -> h
    (x : xs) -> List $ \_ t -> t x (toList xs)

fromList :: List a -> [a]
fromList (List f) = f [] (\h t -> h : fromList t)
```

Note: take some time to admire the simplicity of `fromList`.

Then we need the `Semigroup` `instance` is needed to merge `"Fizz"` and `"Buzz"`:

```haskell
instance Semigroup (List a) where
  List f <> List g = List $ \h t -> f (g h t) (\h' t' -> t h' (t' <> List g))
```

We could redefine `String`:

```haskell
type String' = List Char
```

Additionally, we need `Eq` and `Show`

```haskell
instance Show String' where
  show = show . fromList

instance Eq a => Eq (List a) where
  List f == List g =
    f
      (g True (\_ _ -> False))
      (\h t -> g False (\h' t' -> h == h' && t == t'))
```

Finally, GHC allows defining types instantiated from string symbols.

To do so, we need an `instance` of `IsString`, which has one function, taking
a `String`, so it's easy as:

```haskell
instance IsString String' where
  fromString = toList
```

So far so good, we have a framework to convert Algebraic Data Types to lambda
calculus representation:

* One parameter per constructors
* All parameters are functions ending by the transformed existential type
* The resulting function ends by the transformed existential type

Sadly, we still have one place with pattern-matching:

```haskell
atStream :: Int -> Stream a  -> a
atStream n s
  | n <= 0 = headStream s
  | otherwise = n - 1 `atStream` tailStream s
```

Yes, you read it correctly: `Number`s.

Hopefully, there is a famous way to represent them:
[Church Encoding](https://en.wikipedia.org/wiki/Church_encoding).

The basic idea is: we have a function, which applies `n` times a given function:

```haskell
newtype Natural = Natural (forall x. (x -> x) -> x -> x)
```

So we have:

```haskell
zero = Natural $ \_ x -> x
one = Natural $ \f x -> f x
two = Natural $ \f x -> f (f x)
```

So we can rewrite `atStream`:

```haskell
infixr 4 `atStream'`

atStream' :: Natural -> Stream a  -> a
atStream' (Natural n) s = headStream $ n tailStream s
```

It's way simpler: `tailStream` is applied `n` times, then we fetch `headStream`.

Sadly, we won't implement `subtraction` for the moment, let's adjust `fizzbuzz`:

```haskell
fizzbuzz' :: Natural -> String'
fizzbuzz' n = n `atStream'` error "FizzBuzz isn't defined at 0" .: fizzbuzzStream
```

Finally, as for `String'`, we can implement `Show` (display) and `Num` (basic
numbers operation and converting from symbols, like `IsString`):

```haskell
instance Num Natural where
  Natural m + Natural n = Natural $ \f x -> m f (n f x)
  Natural m * Natural n = Natural $ \f x -> m (n f) x
  abs = id
  signum (Natural n) = Natural $ \f x -> n (const $ f x) x
  fromInteger =
    \case
      0 -> Natural $ \_ x -> x
      n | n < 0 -> error "Church Natural can't be negative"
        | otherwise -> let Natural m = fromInteger (n - 1) in Natural $ \f x -> f (m f x)
  (-) = error "Church Natural can't be subtracted"

instance Show Natural where
  show (Natural f) = show $ f (1 +) 0
```

Note: we have left some parts not implemented, partly due to a more complex
implementation, partly because `Num` is too big and deserves to be split.

And that's it, a pure, end-to-end _fizzbuzz_ branchless.

Done correctly, adding the right constraint, how extreme is it, can improve a
design, even this style improved ergonomics.
