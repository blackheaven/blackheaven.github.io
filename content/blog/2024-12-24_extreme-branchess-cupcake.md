+++
title = "Extreme branchless: Cupcake"
date = 2024-12-24
draft = false
path = "2024-12/extreme-branchless-cupcake"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

Today, the [Cupcake kata](https://codingdojo.org/kata/cupcake/).

This kata aims to introduce OOP's Design Pattern [Decorator](https://en.wikipedia.org/wiki/Decorator_pattern).

In functional programming, it'll be translated into nesting (functions or types/constructors).

We'll use functions in order to be branchless.

Let's import the test cases from the kata description:

```haskell
spec :: Spec
spec =
  describe "Cupcake" $ do
    describe "price" $
      forM_ testCases $ \(name, cake, _, price') ->
        it name $
          cake.price `shouldBe` price'
    describe "price" $
      forM_ testCases $ \(name, cake, cakeName', _) ->
        it name $
          cakeName cake `shouldBe` cakeName'

testCases :: [(String, Cake, String, USDAmount)]
testCases =
  [ ("cupcake", cupcake, "🧁", 100)
  , ("cookie", cookie, "🍪", 200)
  , ("cupcake with chocolate", chocolate cupcake, "🧁 with 🍫", 110)
  , ("cookie with chocolate and nuts", nuts $ chocolate cookie, "🍪 with 🍫 and 🥜", 230)
  , ("cookie with nuts and chocolate", chocolate $ nuts cookie, "🍪 with 🥜 and 🍫", 230)
  , ("cookie with nuts", nuts cookie, "🍪 with 🥜", 220)
  ]
```

Note: we reuse some types we have defined in previous logs:

```haskell
newtype USDAmount = USDAmount {unUSDAmount :: Int}
  deriving newtype (Eq, Ord, Num)

instance Show USDAmount where
  show (USDAmount x) = "$" <> printf "%.2f" (fromIntegral @_ @Float x / 100)

newtype List a = List (forall b. b -> (a -> List a -> b) -> b)

runList :: b -> (a -> List a -> b) -> List a -> b
runList e ne (List f) = f e ne

instance IsList (List a) where
  type Item (List a) = a
  fromList =
    \case
      [] -> List const
      (x : xs) -> List $ \_ t -> t x (fromList xs)
  toList (List f) = f [] (\h t -> h : toList t)

instance Semigroup (List a) where
  List f <> List g = List $ \h t -> f (g h t) (\h' t' -> t h' (t' <> List g))

instance Monoid (List a) where
  mempty = List const
```

We have two functions:

* `price` which is a simple number, it can be freely accumulated
* `cakeName` which as several components:
  + A cake "main" name
  + Followed by `"with topping0 [and topping1 ...]"` when there are topping:
    + Either we build a `String` directly, which would force us to either inspect it, or have a state, or we simply keep a list

```haskell
data Cake = Cake
  { symbol :: String
  , price :: USDAmount
  , toppings :: List String
  }
```

First thing first, let's define the base cakes:

```haskell
cookie :: Cake
cookie =
  Cake {symbol="🍪", price=200, toppings=mempty}

cupcake :: Cake
cupcake =
  Cake {symbol="🧁", price=100, toppings=mempty}
```

Then we can have a type for toppings, which wraps `Cake`:

```haskell
type Topping = Cake -> Cake
```

All `Topping`s are identical: they increment the `price` and add a topping symbol:

```haskell
mkTopping :: String -> USDAmount -> Topping
mkTopping symbol' price' cake = cake { price = cake.price + price', toppings = cake.toppings <> [symbol']}
```

We can then define our toppings as:

```haskell
chocolate :: Topping
chocolate = mkTopping "🍫" 10

nuts :: Topping
nuts = mkTopping "🥜" 20
```

And finally, how `cakeName` is computed:

```haskell
cakeName :: Cake -> String
cakeName cake = cake.symbol <> toppingsName
  where toppingsName = runList "" (\h t -> " with " <> h <> multipleToppings t) cake.toppings
        multipleToppings = runList "" (\h t -> " and " <> h <> multipleToppings t)
```

And, that's it, happy tasting!
