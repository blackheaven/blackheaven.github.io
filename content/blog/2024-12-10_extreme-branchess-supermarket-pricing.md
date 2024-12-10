+++
title = "Extreme branchless: Supermarket Pricing"
date = 2024-12-10
draft = false
path = "2024-12/extreme-branchless-supermarket-pricing"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

This time: [Supermarket pricing](http://codekata.com/kata/kata01-supermarket-pricing/) kata.

While this kata aims to be thought experiment around Money and units, it's an
interesting opportunity to apply some principles describe in the previous logs.

Let's start with the first rule:

> three for a dollar (so what’s the price if I buy 4, or 5?)

The simplest case would be to only represent three items of `$1`:

```haskell
describe "Three for a dollar" $ do
  it "base case" $
    threeForADollar `shouldBe` USDAmount 100

newtype USDAmount
  = USDAmount { unUSDAmount :: Int }
  deriving stock (Eq, Ord)

instance Show USDAmount where
  show (USDAmount x) = "$" <> printf "%.2f" (fromIntegral @_ @Float x / 100)

threeForADollar :: USDAmount
threeForADollar = USDAmount 100
```

Okay, let's get serious, we can add arbitrary numbers:

```haskell
describe "Three for a dollar" $ do
  it "3 is $1" $
    threeForADollar' 3 `shouldBe` USDAmount 100
  it "5 is $2" $
    threeForADollar' 5 `shouldBe` USDAmount 200
  it "6 is $2" $
    threeForADollar' 6 `shouldBe` USDAmount 200
  it "7 is $2.50" $
    threeForADollar' 7 `shouldBe` USDAmount 250
```

We'll introduce two concepts we have seen in previous logs:

```haskell
newtype Natural = Natural (forall x. (x -> x) -> x -> x)

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

data Chained a = Chained
  { current :: a
  , next :: Chained a
  }
```

Note: `Chained` will help us to structure our "behavior"/"strategy" selection as such:

```haskell
threeForADollar0 :: Chained USDAmount
threeForADollar0 =
  Chained {current= USDAmount 0, next=threeForADollar1}

threeForADollar1 :: Chained USDAmount
threeForADollar1 =
  Chained {current= USDAmount 50, next=threeForADollar2}

threeForADollar2 :: Chained USDAmount
threeForADollar2 =
  Chained {current= USDAmount 100, next=threeForADollar3}

threeForADollar3 :: Chained USDAmount
threeForADollar3 =
  Chained {current= USDAmount 100, next=threeForADollar1}
```

Finally, we can come up with a function which moves along the behaviors with the number of items:

```haskell
threeForADollar' :: Natural -> USDAmount
threeForADollar' (Natural n) = (n (.next) threeForADollar0).current
```

And it... does not work:

* `3` gives `$1.00`
* `5` gives `$0.50`
* `6` gives `$1.00`
* `7` gives `$0.50`

Instead, we have to accumulate previous amount:

```haskell
threeForADollar' :: Natural -> USDAmount
threeForADollar' (Natural n) = (n (.next) (threeForADollar0 $ USDAmount 0)).current

threeForADollar0 :: USDAmount -> Chained USDAmount
threeForADollar0 prev =
  Chained {current = prev, next = threeForADollar1 prev}

threeForADollar1 :: USDAmount -> Chained USDAmount
threeForADollar1 prev =
  Chained {current = prev + USDAmount 50, next = threeForADollar2 prev}

threeForADollar2 :: USDAmount -> Chained USDAmount
threeForADollar2 prev =
  Chained {current = prev + USDAmount 100, next = threeForADollar3 prev}

threeForADollar3 :: USDAmount -> Chained USDAmount
threeForADollar3 prev =
  Chained {current = prev + USDAmount 100, next = threeForADollar1 (prev + USDAmount 100)}
```

Note: `threeForADollar1` and `threeForADollar2` does not accumulate on next value, we could have, it's a design choice.

Let's tackle the next rule:

> $1.99/pound (so what does 4 ounces cost?)

We can come up with some tests:

```haskell
describe "Price per pound" $ do
  it "1lbs is $1.99" $
    pricePerPound (lbs 1) `shouldBe` USDAmount 199
  it "4oz is $0.50" $
    pricePerPound (oz 4) `shouldBe` USDAmount 50
```

On a purely practical level, we could either have pounds designed as `Float`/`Double`/`Ratio`,
or as ounces with smart constructors (which will enable us to reuse Church encoding):

```haskell
newtype Lbs
  = Lbs { getOz :: Natural }
  deriving stock (Show)

oz :: Natural -> Lbs
oz = Lbs

lbs :: Natural -> Lbs
lbs = Lbs . (*16)
```

Given that `1 lbs === 16 oz`, we can simply multiply the number of ounces by `1/16` of `$1.99`:

```haskell
pricePerPound :: Lbs -> USDAmount
pricePerPound (Lbs (Natural n)) =
  USDAmount $ ceiling @Double $ fromInteger (numerator ratio) * 199 / fromInteger (denominator ratio)
  where ratio = n (+ (1 % 16)) 0
```

Finally, the last rule of the kata:

> buy two, get one free (so does the third item have a price?)

As a design decision, we'll represent each prices, dropping every `3rd` element:

```haskell
    describe "Buy two, get one free" $ do
      it "Two items only should sum" $
        threeForTwo [USDAmount 100, USDAmount 150] `shouldBe` USDAmount 250
      it "Five items only should sum without the third" $
        threeForTwo [USDAmount 100, USDAmount 150, USDAmount 200, USDAmount 250, USDAmount 300] `shouldBe` USDAmount 800
```

We can reuse our zipping mechanism from [FizzBuzz](@/blog/2024-03-13_fizzbuzz.md):

```haskell
threeForTwo :: [USDAmount] -> USDAmount
threeForTwo = sum . zipWith ($) (cycle [id, id, const (USDAmount 0)])
```
