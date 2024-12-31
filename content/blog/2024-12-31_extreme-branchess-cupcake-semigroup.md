+++
title = "Extreme branchless: Cupcake functional style"
date = 2024-12-31
draft = false
path = "2024-12/extreme-branchless-cupcake-semigroup"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

In my [previous log](@/blog/2024-12-24_extreme-branchess-cupcake.md) we had a
look at the [Cupcake kata](https://codingdojo.org/kata/cupcake/).

I've mentioned that, this kata was thought to practice OOP's Design Pattern
[Decorator](https://en.wikipedia.org/wiki/Decorator_pattern).

Which led to this design:

```haskell
data Cake = Cake
  { symbol :: String
  , price :: USDAmount
  , toppings :: List String
  }

cookie :: Cake
cookie =
  Cake {symbol="🍪", price=200, toppings=mempty}

cupcake :: Cake
cupcake =
  Cake {symbol="🧁", price=100, toppings=mempty}
Then we can have a type for toppings, which wraps `Cake`:

type Topping = Cake -> Cake

mkTopping :: String -> USDAmount -> Topping
mkTopping symbol' price' cake = cake { price = cake.price + price', toppings = cake.toppings <> [symbol']}

chocolate :: Topping
chocolate = mkTopping "🍫" 10

nuts :: Topping
nuts = mkTopping "🥜" 20

cakeName :: Cake -> String
cakeName cake = cake.symbol <> toppingsName
  where toppingsName = runList "" (\h t -> " with " <> h <> multipleToppings t) cake.toppings
        multipleToppings = runList "" (\h t -> " and " <> h <> multipleToppings t)
```

While acceptable, we can argue that `cakeName`:

* is branchless only thanks to `List`
* is quite complex

The root of the problem comes from the constraint to implement an Object-Oriented
Design (Pattern) in a functional language.

OOP has design patterns to workaround technical limitations, FP has mostly
libraries working on structure (and few patterns).

`Cake` does too much, let's start splitting it.

`Cake` should be a basic cake:

```haskell
data Cake = Cake
  { name :: String
  , price :: USDAmount
  }

cookie :: Cake
cookie =
  Cake {name="🍪", price=200}

cupcake :: Cake
cupcake =
  Cake {name="🧁", price=100}
```

Then we have `Toppings`, which represents toppings alone:

```haskell
data Toppings = Toppings
  { name :: String
  , price :: USDAmount
  }

chocolate :: Toppings
chocolate = Toppings {name="🍫", price=10}

nuts :: Toppings
nuts = Toppings {name="🥜", price=20}
```

Finally, we need a way to combine them:

```haskell
data CakeWithToppings = CakeWithToppings
  { name :: String
  , price :: USDAmount
  }

withToppings :: Cake -> Toppings -> CakeWithToppings
withToppings cake toppings = CakeWithToppings {name=cake.name <> " with " <> toppings.name, price=cake.price + toppings.price}
```

It was a bit brutal, if you hadn't noticied, we have moved from `Topping`
to `Toppings`, this allows us to combine it through a `Semigroup`:

```haskell
instance Semigroup Toppings where
  x <> y = Toppings {name=x.name <> " and " <> y.name, price=x.price + y.price}
```

In short:

* Combining two `Toppings` adds a `" and "` and gives a new `Toppings`
* Combining a `Cake` with a `Toppings` adds a `" with "` and gives a `CakeWithToppings`

Entirely type-safe (no risk to add multiple `Toppings` or `Cake`s together),
purely branchless, types-driving.

Notes: to give an idea of the usage, test-cases have been rewritten as such:

```haskell
testCases :: [(String, String, String, USDAmount, USDAmount)]
testCases =
  [ mkTestCase "cupcake" cupcake "🧁" 100
  , mkTestCase "cookie" cookie "🍪" 200
  , mkTestCase "cupcake with chocolate" (cupcake `withToppings` chocolate) "🧁 with 🍫" 110
  , mkTestCase "cookie with chocolate and nuts" (cookie `withToppings` (chocolate <> nuts)) "🍪 with 🍫 and 🥜" 230
  , mkTestCase "cookie with nuts and chocolate" (cookie `withToppings` (nuts <> chocolate)) "🍪 with 🥜 and 🍫" 230
  , mkTestCase "cookie with nuts" (cookie `withToppings` nuts) "🍪 with 🥜" 220
  ]
```

Happy tasting!
