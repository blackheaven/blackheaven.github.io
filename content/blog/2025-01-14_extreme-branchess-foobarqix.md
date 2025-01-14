+++
title = "Extreme branchless: FooBarQix"
date = 2025-01-14
draft = false
path = "2025-01/extreme-branchless-foobarqix"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

This whole series about [branchless started with FizzBuzz](@/blog/2024-03-13_fizzbuzz.md).

Today, I'd like to have a look at [FooBarQix](https://codingdojo.org/kata/FooBarQix/)
which is a variation with the following rules:

* If the number is divisible by 3, write “Foo” instead of the number
* If the number is divisible by 5, add “Bar”
* If the number is divisible by 7, add “Qix”
* For each digit 3, 5, 7, add “Foo”, “Bar”, “Qix” in the digits order

We can come up with some tests:

```haskell
forM_ [(1, "1"), (2, "2"), (3, "FooFoo")] $ \(param, result) ->
  it (show param <> " should be " <> result) $
    foobarqix param `shouldBe` result
it "All multiple of three have 'Foo'" $
  property $ \n ->
    isInfixOf "Foo" $ foobarqix (3 * n)
it "All multiple of five have 'Bar'" $
  property $ \n ->
    isInfixOf "Bar" $ foobarqix (5 * n)
it "All multiple of seven have 'Qix'" $
  property $ \n ->
    isInfixOf "Qix" $ foobarqix (7 * n)
it "All number ending with three should end with 'Foo'" $
  property $ \n ->
    isSuffixOf "Foo" $ foobarqix (10 * n + 3)
it "All number ending with five should end with 'Bar'" $
  property $ \n ->
    isSuffixOf "Bar" $ foobarqix (10 * n + 5)
it "All number ending with seven should end with 'Qix'" $
  property $ \n ->
    isSuffixOf "Qix" $ foobarqix (10 * n + 7)
```

Then we can borrow FizzBuzz design, which does the first part of the rules:

```haskell
foobarqix :: Natural -> String
foobarqix (Natural f) = head $ f tail $ error "0 is not defined" : foobarqixStream

foobarqixStream :: [String]
foobarqixStream = zipWith fromMaybe numbers $ zipWith (<>) foos $ zipWith (<>) bars qixs
  where
    numbers = show <$> [1 ..]
    foos = cycle [Nothing, Nothing, Just "Foo"]
    bars = cycle [Nothing, Nothing, Nothing, Nothing, Just "Bar"]
    qixs = cycle [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just "Qix"]
```

Next step is a little more tricky, we have to split the `Natural` number into digits.

Let's begin with a simple `divMod` (base 10), which, for any `Natural` gives
the `div n 10` and `mod n 10` result:

```haskell
data Chained a = Chained { value :: a, next :: Chained a }

chainedOf :: Natural -> Chained a -> a
chainedOf (Natural n) = (.value) . n (.next)

divMod10_0, divMod10_1, divMod10_2, divMod10_3, divMod10_4, divMod10_5, divMod10_6, divMod10_7, divMod10_8, divMod10_9 :: Natural -> Chained (Natural, Natural)
divMod10_0 u = Chained (u, 0) $ divMod10_1 u
divMod10_1 u = Chained (u, 1) $ divMod10_2 u
divMod10_2 u = Chained (u, 2) $ divMod10_3 u
divMod10_3 u = Chained (u, 3) $ divMod10_4 u
divMod10_4 u = Chained (u, 4) $ divMod10_5 u
divMod10_5 u = Chained (u, 5) $ divMod10_6 u
divMod10_6 u = Chained (u, 6) $ divMod10_7 u
divMod10_7 u = Chained (u, 7) $ divMod10_8 u
divMod10_8 u = Chained (u, 8) $ divMod10_9 u
divMod10_9 u = Chained (u, 9) $ divMod10_0 (u + 1)
```

Then we should somehow iterate on it until we have a list:

```haskell
splitBase10 :: Natural -> [Natural]
splitBase10 n = splitBase10 u <> [mod]
  where (u, mod) = chainedOf n $ divMod10_0 0
```

It somehow does not as there are no base condition.

In order to achieve this, we have to distinguish `0`, returning an empty list:

```haskell
splitBase10 :: Natural -> [Natural]
splitBase10 n = runNatural n (const nonNull) []
  where (u, mod) = chainedOf n $ divMod10_0 0
        nonNull = splitBase10 u <> [mod]
```

Then we need to refactor our main function, so we can distinguish
`number`-string and `FooBarQuix`-string:

```haskell
foobarqix :: Natural -> String
foobarqix n = fromMaybe (show n) $ fetchStr n
  where
    fetchStr n' = head $ runNatural n' tail (Nothing : foobarqixStream)

foobarqixStream :: [Maybe String]
foobarqixStream = zipWith (<>) foos $ zipWith (<>) bars qixs
  where
    foos = cycle [Nothing, Nothing, Just "Foo"]
    bars = cycle [Nothing, Nothing, Nothing, Nothing, Just "Bar"]
    qixs = cycle [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just "Qix"]
```

Finally, we can bind `splitBase10`, giving a `[Natural]` and fetch the
corresponding `Foo`/`Bar`/`Qix`:

```haskell
foobarqix :: Natural -> String
foobarqix n = fromMaybe (show n) $ fetchStr n <> foldMap fetchStr (splitBase10 n)
  where
    fetchStr n' = head $ runNatural n' tail (Nothing : foobarqixStream)

foobarqixStream :: [Maybe String]
foobarqixStream = zipWith (<>) foos $ zipWith (<>) bars qixs
  where
    foos = cycle [Nothing, Nothing, Just "Foo"]
    bars = cycle [Nothing, Nothing, Nothing, Nothing, Just "Bar"]
    qixs = cycle [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just "Qix"]
```

The _step 2_ adds the requirement of tracking `0`.

We could be tempted to simply duplicate `fetchStr`:

```haskell
foobarqix :: Natural -> String
foobarqix n = fromMaybe (show n) $ fetchStr n <> foldMap fetchStrDigit (splitBase10 n)
  where
    fetchStr n' = head $ runNatural n' tail (Nothing : foobarqixStream)
    fetchStrDigit n' = head $ runNatural n' tail $ zipWith (<>) (Just "*" : repeat Nothing) (Nothing : foobarqixStream)
```

It works well... expect when you have only digits (e.g. `101` gives `*` instead of `1*1`),
because `fromMaybe` expect any `Just`.

We should change our tactic:

* have a `*` in our `fromMaybe` default case
* only add a `*` in `Foo`/`Bar`/`Qix` when it's already a `Just`

```haskell
foobarqix :: Natural -> String
foobarqix n =
  fromMaybe (foldMap showDigit digits) $
    (fetchStr n <> foldMap fetchStr digits) *> (fetchStr n <> foldMap fetchStrDigit digits)
  where
    digits = splitBase10 n
    fetchStr n' = head $ runNatural n' tail (Nothing : foobarqixStream)
    fetchStrDigit n' = head $ runNatural n' tail $ zipWith (<>) (Just "*" : repeat Nothing) (Nothing : foobarqixStream)
    showDigit n' = head $ runNatural n' tail ("*" : map return ['1' .. '9'])
```

Note: this code contains two tricks:

* It computes `Foo`/`Bar`/`Qix` twice:
  + A first time without `*`, then it is applied to `*>` which return the right-most value when the left is a `Just`
  + A second time with `*`, which is eventually returned
* The `fromMaybe` default case is not really a `show` anymore, but a mapping/indexed vector to `*123456789`

So far so good, I enjoyed this code kata as, with few variations, it drastically
changes the exercise.
