+++
title = "Extreme branchless: Game of Life"
date = 2024-11-26
draft = false
path = "2024-11/extreme-branchless-gol"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

In my [GDCR Summary](@/blog/2024-11-12_gdcr-summary.md), I have mentioned a [branchless](@/blog/2024-11-05_extreme-branchess-2.md)
of the game of life kata.

This logs attempts to present my solution.

First thing first, we have a `Cell` which can be either `Dead` or `Alive`.

With our previously defined framework it gives the following definition:

```haskell
newtype Cell = Cell (forall a. a -> a -> a)
```

Then we can add few helpers:

```haskell
instance Show Cell where
  show = runCell "Alive" "Dead"

instance Eq Cell where
  (==) = (==) `on` runCell True False

runCell :: a -> a -> Cell -> a
runCell a d (Cell f) = f a d
```

And finally define `dead`/`alive`:

```haskell
alive :: Cell
alive = Cell $ \x _ -> x

dead :: Cell
dead = Cell $ \_ x -> x
```

Then we have to think a bit.

The game of life comes with the following rules:

* A living cell with less than 2 living neighbours die
* A living cell with more than 3 living neighbours die
* A living cell with 2 or three living neighbours live
* A dead cell with three living neighbours becomes alive

Let's see how it goes in a table:

| Number of living neighbours / Initial state | Dead  | Alive |
|---------------------------------------------|-------|-------|
| < 2                                         | Dead  | Dead  |
| 2                                           | Dead  | Alive |
| 3                                           | Alive | Alive |
| > 3                                         | Dead  | Dead  |

Taking the number of living neighbours as main inputs helps us to pick
a `Strategy` which could be represented as:

```haskell
newtype Strategy = Strategy
  { envolve :: Cell -> Cell
  }
```

We should improve it, especially because at some point, we would need to count
the neighbours, as we have seen in the previous log, the best way to deal with
numbers is to use [Church encoding](https://en.wikipedia.org/wiki/Church_encoding).

To do so, each `Strategy` should be able to know the next `Strategy`, the `Strategy`
with one more living neighbours (it's closed to OOP's [Chain of responsibility](https://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) design pattern):

```haskell
data Strategy = Strategy
  { name :: String
  , envolve :: Cell -> Cell
  , nextStrategy :: Strategy
  }
```

Then, we can define the strategies according to our table:

```haskell
starvation0 :: Strategy
starvation0 =
  Strategy {
    name = "Starving",
    envolve = const dead,
    nextStrategy = starvation1
  }

starvation1 :: Strategy
starvation1 =
  Strategy {
    name = "Starving",
    envolve = const dead,
    nextStrategy = equilibrium
  }

equilibrium :: Strategy
equilibrium =
  Strategy {
    name = "Equilibrium",
    envolve = id,
    nextStrategy = reproduction
  }

reproduction :: Strategy
reproduction =
  Strategy {
    name = "Reproduction",
    envolve = const alive,
    nextStrategy = overpopulation
  }

overpopulation :: Strategy
overpopulation =
  Strategy {
    name = "Overpopulation",
    envolve = const dead,
    nextStrategy = overpopulation
  }
```

Note:

* `const x` returns a function which always returns `x`, whatever the input is
* `id`, is the identity function, returning the given parameter

It looks like this:

{% mermaid() %}
graph LR;
    starvation0 --> starvation1;
    starvation1 --> equilibrium;
    equilibrium --> reproduction;
    reproduction --> overpopulation;
    overpopulation --> overpopulation;
{% end %}

Then we need a way to evolve a whole grid.

There are several ways to represent a grid, I have chosen a `Set Position`,
which represents the living cells:

```haskell
type Position = (Int, Int)

envolveGrid :: Set.Set Position -> Set.Set Position
```

The approach is straightforward:

* For each living cell: list their neighbours
* For each neighbours count their neighbours and select the correct `Strategy`
* Give to the `Strategy` the current neighbour state

Which gives:

```haskell
envolveGrid :: Set.Set Position -> Set.Set Position
envolveGrid initialAliveCells = Set.filter isAliveEnvolved $ foldMap listNeighbours initialAliveCells
  where isAliveEnvolved :: Position -> Bool
        isAliveEnvolved pos = runCell True False $ (selectStrategy pos).envolve $ isAlive pos alive dead
        isAlive :: Position -> a -> a -> a
        isAlive pos whenAlive whenDead = thenElseIf whenAlive whenDead $ toBool' $ Set.member pos initialAliveCells
        selectStrategy :: Position -> Strategy
        selectStrategy = foldl (\acc cell -> isAlive cell (.nextStrategy) id acc) starvation0 . listNeighbours
        listNeighbours (x, y) =
          Set.fromList [
            (x + xDiff, y + yDiff)
            | xDiff <- [-1 .. 1]
            , yDiff <- [-1 .. 1]
            , xDiff /= 0 || yDiff /= 0
          ]
```

Notes:

* `foldMap listNeighbours initialAliveCells` collects all neighbours
* `Set.filter isAliveEnvolved` keep only `Cell` still alive in next generation
* `isAliveEnvolved` apply the selected `Strategy` with current neighbour's state and convert it to `Bool` for `filter`
* `isAlive` check if a cell is alive in the current grid, there's much boilerplate because I use the usual `Set`, I have to cheat a bit here
* `selectStrategy` list the potential neighbours and on them, starting from a 0-neighbour strategy, for each `Position`, if the `Cell` is dead, we keep the current `Strategy`, otherwise, pick the `nextStrategy`
* `listNeighbours` generates `+1`/`-1` `Position`

{% mermaid() %}
graph TD;
    Position -- is alive --> nextStrategy
    Position -- is dead --> id
{% end %}

And that's it.

Note: here's my `Bool'` wrapper:

```haskell
newtype Bool' = Bool' (forall a. a -> a -> a)

instance Show Bool' where
  show = thenElseIf "True" "False"

instance Eq Bool' where
  (==) = (==) `on` thenElseIf True False

thenElseIf :: a -> a -> Bool' -> a
thenElseIf t f (Bool' g) = g t f

toBool' :: Bool -> Bool'
toBool' x = if x then true else false

true :: Bool'
true = Bool' $ \x _ -> x

false :: Bool'
false = Bool' $ \_ x -> x
```
