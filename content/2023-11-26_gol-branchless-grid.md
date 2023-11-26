+++
title = "Game of Like kata: branchless Grid"
date = 2022-11-26
draft = false
path = "2023-11/gol-branchless-grid"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo"]
+++

In my [previous log](@/2023-11-22_tdd-optimizing-for-inputs-outputs.md)
I have tackled [Conway's Game Of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

In conclusion, I have stated that we could implement the full kata (i.e. even
the grid) without branches (no `if`/`else`/`case`/`pattern matching`, so no sum types).

As a reminder, we ended up with this:

```haskell
-- ...
describe "Reproduction (three live neighbours)" $ do
  it "Any dead cell with exactly three live neighbours becomes a live cell" $
    reproduction.next Dead `shouldBe` Alive
  it "Any live cell with three live neighbours lives on to the next generation" $
    reproduction.next Alive `shouldBe` Alive
describe "Overpopulation (more than three live neighbours)" $ do
  it "Any live cell with more than three live neighbours dies" $
    overpopulation.next Alive `shouldBe` Dead
describe "Survive (two live neighbours)" $ do
  it "Any Dead cell with fewer than three live neighbours stays dead on to the next generation" $
    survive.next Dead `shouldBe` Dead
  it "Any dead cell with exactly three live neighbours becomes a live cell" $
    survive.next Alive `shouldBe` Alive
describe "Underpopulation (zero or one live neighbours)" $ do
  it "Any Dead cell with fewer than three live neighbours stays dead on to the next generation" $
    underpopulation.next Dead `shouldBe` Dead
  it "Any live cell with fewer than two live neighbours dies" $
    underpopulation.next Alive `shouldBe` Dead
-- ...

data Cell
  = Alive
  | Dead
  deriving stock (Eq, Show)

newtype Neighbours
  = Neighbours { getNeighbours :: Int }
  deriving newtype (Eq, Ord, Show, Num)

newtype Neighbourhood
  = Neighbourhood { next :: Cell -> Cell }

reproduction :: Neighbourhood
reproduction = Neighbourhood $ const Alive

overpopulation :: Neighbourhood
overpopulation = Neighbourhood $ const Dead

survive :: Neighbourhood
survive = Neighbourhood id

underpopulation :: Neighbourhood
underpopulation = Neighbourhood $ const Dead
```

Let's start with the neighbourhood selection.

If I have to draft things up, I would come up with an implementation like this:

```haskell
neighbourhood :: Int -> Neighbourhood
neighbourhood =
  \case
    0 -> underpopulation
    1 -> underpopulation
    2 -> survive
    3 -> reproduction
    _ -> overpopulation
```

But there are two problems:

1. There is branching
2. I have no direct way to test it directly (I could test the function with its behavior, but it's not handy)

Note: here is the limit of the kata, in production code I would keep the
here-above implementation while testing the functions.

Let's tackle the testability issue through refactoring adding the neighbourhood
name:

```haskell
data Neighbourhood = Neighbourhood
  { name :: String
  , next :: Cell -> Cell
  }

reproduction :: Neighbourhood
reproduction =
  Neighbourhood
    { name = "reproduction"
    , next = const Alive
    }

overpopulation :: Neighbourhood
overpopulation =
  Neighbourhood
    { name = "overpopulation"
    , next = const Dead
    }

survive :: Neighbourhood
survive =
  Neighbourhood
    { name = "survive"
    , next = id
    }

underpopulation :: Neighbourhood
underpopulation =
  Neighbourhood
    { name = "underpopulation"
    , next = const Dead
    }
```

Then, instead of taking a number for the function, let's take a `Cell`s list

```haskell
-- ...
it "No alive neighbours should be 'underpopulation'" $
  (neighbourhood []).name `shouldBe` "underpopulation"
-- ...

neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = const underpopulation
```

Then we need to test `survive` (2):

```haskell
-- ...
it "Two alive neighbours should be 'survive'" $
  (neighbourhood [Alive, Alive]).name `shouldBe` "survive"
-- ...

data Neighbourhood = Neighbourhood
  { name :: String
  , next :: Cell -> Cell
  , nextNeighbourhood :: Neighbourhood
  }

underpopulation0 :: Neighbourhood
underpopulation0 =
  Neighbourhood
    { name = "underpopulation"
    , next = const Dead
    , nextNeighbourhood = underpopulation1
    }

underpopulation1 :: Neighbourhood
underpopulation1 =
  Neighbourhood
    { name = "underpopulation"
    , next = const Dead
    , nextNeighbourhood = survive
    }

survive :: Neighbourhood
survive =
  Neighbourhood
    { name = "survive"
    , next = id
    , nextNeighbourhood = reproduction
    }

reproduction :: Neighbourhood
reproduction =
  Neighbourhood
    { name = "reproduction"
    , next = const Alive
    , nextNeighbourhood = overpopulation
    }

overpopulation :: Neighbourhood
overpopulation =
  Neighbourhood
    { name = "overpopulation"
    , next = const Dead
    , nextNeighbourhood = overpopulation
    }

neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = foldr (const (.nextNeighbourhood)) underpopulation0
```

So, a lot of things went on here:

* For each `Neighbourhood`, there is a `nextNeighbourhood` specified (which is the `Neighbourhood` when there is one more `Alive` `Cell`, it is what's called [Church Encoding](https://en.wikipedia.org/wiki/Church_encoding))
* I have split `underpopulation` into `underpopulation0` and `underpopulation1` to represent `0` and `1` `Alive` `Cell`

There's have few more coverage tests:

```haskell
it "No alive neighbours should be 'underpopulation'" $
  (neighbourhood []).name `shouldBe` "underpopulation"
it "One alive neighbours should be 'underpopulation'" $
  (neighbourhood [Alive]).name `shouldBe` "underpopulation"
it "Two alive neighbours should be 'survive'" $
  (neighbourhood [Alive, Alive]).name `shouldBe` "survive"
it "Three alive neighbours should be 'reproduction'" $
  (neighbourhood [Alive, Alive, Alive]).name `shouldBe` "reproduction"
it "Four alive neighbours should be 'overpopulation'" $
  (neighbourhood [Alive, Alive, Alive, Alive]).name `shouldBe` "overpopulation"
```

Good, now, we are able to break things dealing with `Dead` `Cell`.

```haskell
-- ...
it "Two alive neighbours and three dead should be 'survive'" $
  (neighbourhood [Dead, Alive, Dead, Alive, Dead]).name `shouldBe` "survive"
-- ...

neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = foldr go underpopulation0
  where go =
          \case
            Alive -> (.nextNeighbourhood)
            Dead -> id
```

Here we are:

* On `Dead` `Cell`, keep the `Neighbourhood`
* On `Alive` `Cell`, go to the `nextNeighbourhood`

But, but, but, conditions are back with the `parttern matching`, we have to
rework the `Cell` (using _Church Encoding_ again):

```haskell
newtype Cell = Cell (forall a. forall a. a -> a -> a)

instance Show Cell where
  show = runCell "Dead" "Alive"

instance Eq Cell where
  x == y = runCell (Left ()) (Right ()) x == runCell (Left ()) (Right ()) y

runCell :: a -> a -> Cell -> a
runCell d a (Cell f) = f d a

alive :: Cell
alive = Cell $ \_ a -> a

dead :: Cell
dead = Cell const
```

I have to admit that's the first time I have written `Eq` and `Show` instances
for a function-based type (and I'm glad it went so well).

Finally we can rewrite `neighbourhood`:

```haskell
neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = foldr (runCell id (.nextNeighbourhood)) underpopulation0
```

Note: at this point I find the code way more brittle than a
`case`/`pattern matching`.
That's what happen when you have to parameters with the same type but which
position is important (i.e. are not interchangeable without changing the output).

Let's quickly improve that:

```haskell
newtype WhenDead a = WhenDead a

newtype WhenAlive a = WhenAlive a

runCell :: WhenDead a -> WhenAlive a -> Cell -> a
runCell (WhenDead d) (WhenAlive a) (Cell f) = f d a

neighbourhood :: [Cell] -> Neighbourhood
neighbourhood = foldr (runCell (WhenDead id) (WhenAlive (.nextNeighbourhood))) underpopulation0
```

A bit verbose, but less error prone.

We can now finally proceed to the `Grid`.

For simplicity reasons, instead of an infinite `Grid`, we'll have a finite one.

Then we'll start with a simple design:

* `Grid` will be opaque (meaning the constructor won't be exported, though it'll be a `Map`)
* Providing going back-and-worth `Grid` through a `Set Pos` for `Alive` `Cell`

For the tests, I have chosen [the blinker](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)
on a 3x3 `Grid` for our test cases:

```haskell
describe "Grid" $ do
  let verticalBlinkerCells = [Pos 1 0, Pos 1 1, Pos 1 2]
  it "Vertical blinker should become horizontal" $
    aliveCells (nextGrid $ mkGrid (Pos 2 2) verticalBlinkerCells)
      `shouldBe` [Pos 0 1, Pos 1 1, Pos 2 1]
  it "Vertical blinker should become vertical after two generations" $
    aliveCells (nextGrid $ nextGrid $ mkGrid (Pos 2 2) verticalBlinkerCells)
      `shouldBe` verticalBlinkerCells
```

Let's start with types:

```haskell
data Pos = Pos
  { posX :: Int,
    posY :: Int
  }
  deriving stock (Eq, Ord, Show)

newtype Grid
  = Grid { getGrid :: Map.Map Pos Cell }
  deriving stock (Eq, Show)
```

Then the helpers:

```haskell
mkGrid :: Pos -> Set.Set Pos -> Grid
mkGrid limits alives =
  Grid $
    Map.fromList
    [ let p = Pos x y in (p, if Set.member p alives then alive else dead)
      | x <- [0 .. limits.posX]
      , y <- [0 .. limits.posY]
    ]

aliveCells :: Grid -> Set.Set Pos
aliveCells =
  Set.fromList
  . mapMaybe (\(p, c) -> runCell (WhenDead Nothing) (WhenAlive $ Just p) c)
  . Map.toList
  . getGrid
```

And finally the function to evolve the `Grid`:

```haskell
nextGrid :: Grid -> Grid
nextGrid (Grid grid) =
  Grid $
    Map.mapWithKey (\p -> (neighbourhood $ neighbours p).next) grid
  where neighbours (Pos x y) =
          [ Map.findWithDefault dead (Pos (x + dx) (y + dy)) grid
            | dx <- [(-1) .. 1]
            , dy <- [(-1) .. 1]
            , dx /= 0 || dy /= 0
          ]
```

And we're done.

Sure, it's not perfect, especially it will be slow on wide `Grid` with few
`Alive` `Cell`, but I think it's acceptable for a code kata.
