+++
title = "Extreme branchless: Retrospective"
date = 2025-03-11
draft = false
path = "2025-03/extreme-branchless-retrospective"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming", "retrospective"]
+++

3 months ago, I have started a [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey,
the goal was to push the branchless contraint to its limits, I have applied to
the following 14 katas:

* [Game of Life](@/blog/2024-11-26_extreme-branchess-gol.md)
* [Mars Rover](@/blog/2024-12-03_extreme-branchess-rover.md)
* [Supermarket pricing](@/blog/2024-12-10_extreme-branchess-supermarket-pricing.md)
* [Bowling kata](@/blog/2024-12-17_extreme-branchess-bowling.md)
* [Cupcake](@/blog/2024-12-24_extreme-branchess-cupcake.md)
* [Cupcake](@/blog/2024-12-31_extreme-branchess-cupcake-semigroup.md)
* [FooBarQix](@/blog/2025-01-14_extreme-branchess-foobarqix.md)
* [Gilded rose](@/blog/2025-01-21_extreme-branchess-gildedrose-1.md)
* [Gilded rose](@/blog/2025-02-04_extreme-branchess-gildedrose-2.md)
* [Langton Ant](@/blog/2025-02-11_extreme-branchess-langton-ant.md)
* [Mastermind](@/blog/2025-02-18_extreme-branchess-mastermind.md)
* [Minesweeper](@/blog/2025-02-25_extreme-branchess-minesweeper.md)
* [Pagination Seven](@/blog/2025-03-04_extreme-branchess-pagination-seven.md)

Few takeaways:

* Branchless is engineered, not discovered

Taking [Pagination Seven](@/blog/2025-03-04_extreme-branchess-pagination-seven.md)
as an example, the main tactic used here was to store rendering strategies in
lists and picking the right one according to the total/current page.

It takes a conscious effort to not use a `if`/`then`/`else` statement.

* Branchless can spread responsibilities, reducing cohesion

We can use [Mars Rover](@/blog/2024-12-03_extreme-branchess-rover.md) to show it:

```haskell
north :: Direction
north =
  Direction
  { displayDirection = "N"
  , nextRotateLeft = west
  , nextRotateRight = east
  , moveForward = \position -> position {y = position.y + 1}
  }

east :: Direction
east =
  Direction
  { displayDirection = "E"
  , nextRotateLeft = north
  , nextRotateRight = south
  , moveForward = \position -> position {x = position.x + 1}
  }

south :: Direction
south =
  Direction
  { displayDirection = "S"
  , nextRotateLeft = east
  , nextRotateRight = west
  , moveForward = \position -> position {y = position.y - 1}
  }

west :: Direction
west =
  Direction
  { displayDirection = "W"
  , nextRotateLeft = south
  , nextRotateRight = north
  , moveForward = \position -> position {x = position.x - 1}
  }
```

With a sum type, we would have 4 functions, pattern-matching on each constructor.

I think it's a tradeoff, it could be a good solution if you are in a really
open/extendable/dynamic context.

* Branchless can make simple thing hard, but it can also make it simpler

If you had a look at [FooBarQix](@/blog/2025-01-14_extreme-branchess-foobarqix.md),
you may recall my trick to had division:

```haskell
splitBase10 :: Natural -> [Natural]
splitBase10 n = splitBase10 u <> [mod]
  where (u, mod) = chainedOf n $ divMod10_0 0

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

I reworked it on my own a bit later to:

```haskell
splitBase10 :: Natural -> [Natural]
splitBase10 n = runNatural n (const nonNull) []
  where (u, mod) = head $ runNatural n tail $ zip (concatMap (replicate 10) [0..]) (cycle [0..9])
        nonNull = splitBase10 u <> [mod]
```

* Branchless can be derived mechanically

There's actually two ways to go branchless:

From algebraic data-types (ADT):

```haskell
data T = A Int | B String Bool

-- Becomes
newtype T = T { runT :: forall a. (Int -> a) -> (String -> Bool -> a) -> a }
```

Or from control structures:

```haskell
agedBrie (sellIn - 1) . min 50 $
  if sellIn < 1
  then quality + 2
  else quality + 1

-- Becomes
agedBrie (sellIn - 1) . min 50 $
  ((quality + 2) : repeat (quality + 1)) `at` sellIn
```

* I won't use it daily, kind of

Brancheless is not something we see in really-world code bases, it's the opposite.

We have plenty of nested conditionals, which comes from, either a lack of
understanding for the product team, or a specifically requests from the users
or the business units.

However, doing these katas has shifted my mind on software design, like I wasn't
for years, and my designing style has changed since.
