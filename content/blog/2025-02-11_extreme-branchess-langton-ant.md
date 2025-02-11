+++
title = "Extreme branchless: Langton Ant"
date = 2025-02-11
draft = false
path = "2025-02/extreme-branchless-langton-ant"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

This time: [Langton Ant](https://codingdojo.org/kata/LangtonAnt/) kata.

It's not a kata I'm used to doing, it has only two rules:

* At a white square, turn 90° right, flip the color of the square, move forward one unit
* At a black square, turn 90° left, flip the color of the square, move forward one unit

At a bird's eye view it's a [Mar Rover](@/blog/2024-12-03_extreme-branchess-rover.md).

Let's start with our test cases:

```haskell
describe "Langton Ant" $ do
  forM_ [
      (white, north, black, east)
    , (white, east, black, south)
    , (white, south, black, west)
    , (white, west, black, north)
    , (black, north, white, west)
    , (black, west, white, south)
    , (black, south, white, east)
    , (black, east, white, north)
    ] $ \(startSquare, startDirection, expectedSquare, expectedDirection) ->
      it ("when facing " <> show startDirection <> " on a " <> show startSquare <> " square should face " <> show expectedDirection <> " on a " <> show expectedSquare <> " square") $
        next (startSquare, startDirection) `shouldBe` (expectedSquare, expectedDirection)
```

We have few cycles:

* Square (color)
* Direction (on both rotations)

We can reuse our chaining tactics.

First on directions:

```haskell
data Direction = Direction
  { name :: String
  , rotateLeft :: Direction
  , rotateRight :: Direction
  }

instance Show Direction where
  show = (.name)

instance Eq Direction where
  (==) = (==) `on` (.name)
```

We have:

* a name (mainly for display and tests)
* the result of a left rotation
* the result of a right rotation

We can define each `Direction`s:

```haskell
north :: Direction
north = Direction
  { name = "north"
  , rotateLeft = west
  , rotateRight = east
  }

south :: Direction
south = Direction
  { name = "south"
  , rotateLeft = east
  , rotateRight = west
  }

west :: Direction
west = Direction
  { name = "west"
  , rotateLeft = south
  , rotateRight = north
  }

east :: Direction
east = Direction
  { name = "east"
  , rotateLeft = north
  , rotateRight = south
  }
```

Then, we have the `Square`:

```haskell
data Square = Square
  { name :: String
  , flip :: Square
  , rotate :: Direction -> Direction
  }

instance Show Square where
  show = (.name)

instance Eq Square where
  (==) = (==) `on` (.name)
```

It's a bit more complex to design:

* a name (mainly for display and tests)
* the result of a flip
* a way to fetch the result of the correct rotation

We can define our two squares:

```haskell
black :: Square
black = Square
  { name = "black"
  , flip = white
  , rotate = (.rotateLeft)
  }

white :: Square
white = Square
  { name = "white"
  , flip = black
  , rotate = (.rotateRight)
  }
```

Finally, we can compose everything:

```haskell
next :: (Square, Direction) -> (Square, Direction)
next (square, direction) = (square.flip, square.rotate direction)
```

At this point, the process is simple: the new `Square` is the flipped result,
and the new `Direction` is the rotation result selected by the `Square`
`rotate` strategy.

We could add an extra color, it would only take to a link in the chain:

```haskell
red :: Square
red = Square
  { name = "red"
  , flip = white
  , rotate = id
  }
```

Note: `id` will keep current `Direction`
