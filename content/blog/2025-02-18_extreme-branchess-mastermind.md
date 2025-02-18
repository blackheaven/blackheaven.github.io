+++
title = "Extreme branchless: Mastermind"
date = 2025-02-18
draft = false
path = "2025-02/extreme-branchless-mastermind"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

This time: [Mastermind](https://codingdojo.org/kata/Mastermind/) kata.

It works as follows:

* A codemaker choose a secret (4  colored pegs, among 6 colors)
* A codecracker mke a guess
* The codemaker gives the number of well-placed and misplaced colors

Let's bootstrap some tests:

```haskell
describe "Mastermind" $ do
  forM_ [
      (["blue"], ["blue"], (1, 0))
    ] $ \(secret, guess, expected) ->
      it ("with secret " <> show secret <> " guessing " <> show guess <> " should evaluate to " <> show expected) $
        evaluate secret guess `shouldBe` expected
```

We can hard-code the response:

```haskell
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess = (1, 0)

newtype Color
  = Color String
  deriving newtype (Eq, Ord, Show, IsString)
```

Then, we can add one color well-placed, and get the length to make it pass:

```haskell
-- (["blue", "red"], ["blue", "red"], (2, 0))
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess = (length secret, 0)
```

The next interesting use-case is to have a color not present in the secret,
filtering is simple to make it work:

```haskell
-- (["blue", "red"], ["blue", "yellow"], (1, 0))
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess = (length $ filter (uncurry (==)) joined, 0)
  where joined = zip secret guess
```

Note: `zip` will "join" `secret`/`guess` element by element in tuples,
then we can compare them one by one.

We have to tackle misplaced colors:

```haskell
-- (["blue", "red"], ["yellow", "blue"], (0, 1))
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess = (length wellPlaced, length misplaced)
  where joined = zip secret guess
        wellPlaced = filter (uncurry (==)) joined
        (extraSecret, extraGuess) = unzip $ filter (uncurry (/=)) joined
        misplaced = filter (`elem` extraSecret) extraGuess
```

The first thing is to collect non-matching colors.
We then separate non-matching `secret`/`guess` elements with `unzip`.
Finally, count the number of wrong `guess` are in remaining `secret`s.

Then, we have to check that, each misplaced color, has the same number of
remaining `secret`s:

```haskell
-- (["blue", "blue", "orange", "orange"], ["blue", "yellow", "blue", "blue"], (1, 1))
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess = (length wellPlaced, missplacedCount)
  where joined = zip secret guess
        wellPlaced = filter (uncurry (==)) joined
        (extraSecret, extraGuess) = unzip $ filter (uncurry (/=)) joined
        toMap = Map.fromListWith (+) . flip zip (repeat 1)
        missplacedCount = sum $ Map.elems $ Map.intersectionWith min (toMap extraSecret) (toMap extraGuess)
```

The best way to achieve this is to rely on a `Map Color`, count the number of
elements on each side and join by `min`.

This implementation is short enough, but there are many branches in the
functions I have used.

Let's break this down, instead of filtering elements, we can accumulate
a partial result we would be able to combined later, such as:

```haskell
type Accumulator = (Sum Int, (Misplaced, Misplaced))
```

`Misplaced` contains a list, color-by-color count of misplaced elements:

```haskell
data Misplaced = Misplaced
  { blues :: Int
  , reds :: Int
  , yellows :: Int
  , oranges :: Int
  , greens :: Int
  , purples :: Int
  }
  deriving stock (Show)

instance Semigroup Misplaced where
  x <> y =
    Misplaced
      { blues = x.blues + y.blues
      , reds = x.reds + y.reds
      , yellows = x.yellows + y.yellows
      , oranges = x.oranges + y.oranges
      , greens = x.greens + y.greens
      , purples = x.purples + y.purples
      }

instance Monoid Misplaced where
  mempty =
    Misplaced
      { blues = 0
      , reds = 0
      , yellows = 0
      , oranges = 0
      , greens = 0
      , purples = 0
      }
```

We should add a function to get the definitive count:

```haskell
countMisplaced :: Misplaced -> Misplaced -> Int
countMisplaced x y =
  min x.blues y.blues
  + min x.reds y.reds
  + min x.yellows y.yellows
  + min x.oranges y.oranges
  + min x.greens y.greens
  + min x.purples y.purples
```

Then, we should redesign `Color`, so it includes:

* A function to get an `Accumulator`, on a comparison with another `Color`
* An `Accumulator` per `Color`

Which gives this definition:

```haskell
data Color = Color
  { name :: String
  , compareColor :: Color -> Accumulator
  , onBlue :: Misplaced -> Accumulator
  , onRed :: Misplaced -> Accumulator
  , onYellow :: Misplaced -> Accumulator
  , onOrange :: Misplaced -> Accumulator
  , onGreen :: Misplaced -> Accumulator
  , onPurple :: Misplaced -> Accumulator
  }

instance Show Color where
  show = (.name)
```

Then we can build each `Color`, defaulting to errors and overriding fields
according to their color:

```haskell
mkColor :: String -> Misplaced -> (Color -> Misplaced -> Accumulator) -> (Color -> Color) -> Color
mkColor name' m access alter =
  alter $
    Color
      { name = name'
      , compareColor = \other -> access other m
      , onBlue = defaultOn
      , onRed = defaultOn
      , onYellow = defaultOn
      , onOrange = defaultOn
      , onGreen = defaultOn
      , onPurple = defaultOn
      }
  where defaultOn m' = (Sum 0, (m, m'))

matchingAccumulator :: Misplaced -> Accumulator
matchingAccumulator _ = (Sum 1, (mempty, mempty))
```

Which gives:

```haskell
blue, red, yellow, orange, green, purple :: Color
blue = mkColor "blue" (mempty { blues = 1 }) (.onBlue) (\x -> x { onBlue = matchingAccumulator })
red = mkColor "red" (mempty { reds = 1 }) (.onRed) (\x -> x { onRed = matchingAccumulator })
yellow = mkColor "yellow" (mempty { yellows = 1 }) (.onYellow) (\x -> x { onYellow = matchingAccumulator })
orange = mkColor "orange" (mempty { oranges = 1 }) (.onOrange) (\x -> x { onOrange = matchingAccumulator })
green = mkColor "green" (mempty { greens = 1 }) (.onGreen) (\x -> x { onGreen = matchingAccumulator })
purple = mkColor "purple" (mempty { purples = 1 }) (.onPurple) (\x -> x { onPurple = matchingAccumulator })
```

Let's use it in our tests:

```haskell
spec :: Spec
spec =
  describe "Mastermind" $ do
    forM_ [
        ([blue], [blue], (1, 0))
      , ([blue, red], [blue, red], (2, 0))
      , ([blue, red], [blue, yellow], (1, 0))
      , ([blue, red], [yellow, blue], (0, 1))
      , ([blue, red, orange], [blue, yellow, blue], (1, 0))
      , ([blue, blue, orange, orange], [blue, yellow, blue, blue], (1, 1))
      ] $ \(secret, guess, expected) ->
        it ("with secret " <> show secret <> " guessing " <> show guess <> " should evaluate to " <> show expected) $
          evaluate secret guess `shouldBe` expected
```

Finally, let's rewrite `evaluate`:

```haskell
evaluate :: [Color] -> [Color] -> (Int, Int)
evaluate secret guess =
  bimap getSum (uncurry countMisplaced)
    $ mconcat
    $ zipWith (.compareColor) secret guess
```

It works as follows:

* We compare element by element with the built-in function
* We get a list of `Accumulator`s we combine into one though `mconcat` (which relies on `Monoid`)
* Finally, we unwrap the well-placed `Sum` and `countMisplaced` total

Here we are!
