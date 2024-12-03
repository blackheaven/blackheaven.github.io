+++
title = "Extreme branchless: Mars Rover"
date = 2024-12-03
draft = false
path = "2024-12/extreme-branchless-rover"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey,
I wanted to refactor a highly state-based kata: the [Mars Rover](@/blog/2023-11-05_gdcr-summary.md).

As a reminder, we have a state defined as:

```haskell
data Rover = Rover
  { position :: Position,
    direction :: Direction
  }

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving stock (Eq)

data Direction = North | East | South | West
```

Needless to say that `Direction` creates a lot of branching:

```haskell
roverCommands :: String -> String
roverCommands = displayPosition . foldl' go (Rover {x = 0, y = 0, direction = North})
  where
    go p =
      \case
        'R' ->
          p
            { direction =
                case p.direction of
                  North -> East
                  East -> South
                  South -> West
                  West -> North
            }
        'L' ->
          p
            { direction =
                case p.direction of
                  North -> West
                  East -> North
                  South -> East
                  West -> South
            }
        'M' ->
          let potentialPosition =
                case p.direction of
                  North -> (p.position) {y = p.position.y + 1}
                  East -> (p.position) {x = p.position.x + 1}
                  South -> (p.position) {y = p.position.y - 1}
                  West -> (p.position) {x = p.position.x - 1}
           in p {position = fromMaybe p.position $ nextPosition potentialPosition}
        c -> error $ "Unknown command " <> show c
```

We have two patterns matching here: `Direction` and `Command`.

For `Direction`, we can reuse [Game of Life `Strategy` tactic](@/blog/2024-11-26_extreme-branchess-gol.md),
cross-referencing `Direction` and defining `Position` evolution:

```haskell
data Direction = Direction
  { displayDirection :: String
  , nextRotateLeft :: Direction
  , nextRotateRight :: Direction
  , moveForward :: Position -> Position
  }
```

We can define each `Direction`:

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

The next step, we have to design the `Command` so they'll rely on `Direction`
instead of pattern matching:

```haskell
newtype Command = Command { runCommand :: Rover -> Rover }
```

A `Command` is a simple function which evolve a `Rover`.

We can define them individually:

```haskell
cmdLeft :: Command
cmdLeft =
  Command $ \rover -> rover { direction = rover.direction.nextRotateLeft }

cmdRight :: Command
cmdRight =
  Command $ \rover -> rover { direction = rover.direction.nextRotateRight }

cmdMove :: (Position -> Maybe Position) -> Command
cmdMove nextPosition =
  Command $ \rover ->
    rover { position = fromMaybe rover.position $ nextPosition $ rover.direction.moveForward rover.position }
```

It's a straightforward delegation to `Direction`, no more logic is needed.

Finally, we can compose everything in our entry-point function:

```haskell
roverCommands :: (Position -> Maybe Position) -> String -> String
roverCommands nextPosition =
  displayPosition
    . foldl' (\rover c -> (parseCommand c).runCommand rover)
        (Rover {position = Position {x = 0, y = 0}, direction = north})
  where
    parseCommand =
      \case
        'R' -> cmdRight
        'L' -> cmdLeft
        'M' -> cmdMove nextPosition
        c -> error $ "Unknown command " <> show c
```

Note: we have kept `parseCommand` because one of the aim of the kata is to
practice [Outside-in TDD](https://8thlight.com/insights/tdd-from-the-inside-out-or-the-outside-in),
keeping a stable interface.

In a real-world codebase, I would rework it so split responsibilities:

```haskell
roverCommands :: [Command] -> Rover
roverCommands = ...
```
