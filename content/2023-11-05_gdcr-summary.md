+++
title = "GDCR 2023: My participation summary"
date = 2022-11-05
draft = false
path = "2023-11/gdcr-summary"

[taxonomies]
categories = ["Code practice"]
tags = ["gdcr", "code kata", "coding dojo", "code retreat"]
+++

[As every year](@/2022-11-06_gdcr-summary.md), the [Global day of code retreat](https://www.coderetreat.org/events/) is a good excuse to work and rework the same kata a full day.

This year I have attended to the one organized by the [Software Crafters Lyon](https://twitter.com/swcraftlyon)
(I'm a member of the organizing team, but I didn't organize the Code Retreat this time).

## Subject

It was [Codurance's Mars Rover Kata](https://www.codurance.com/katas/mars-rover).

I have previously attempted it multiple times in the past, but I did not enjoy it.

## My iterations

1.
  * Language: Python
  * Paired with a freshman student
  * It was an exploratory session
  * During the introduction, he enumerated a long list of programming he knew, but since he did not have a configured development environment I have chosen Python as I knew it comes with a testing library
  * We focused on the rover: making it move forward, teleport at boundaries
  * I feel it was more an introduction to industry development
2.
  * Language: Java
  * Paired with a junior developer
  * Additional constraints: Immutable and/or Primitive Obsession
  * We focused on the rover: making it move forward, teleport at boundaries, rotate
  * We chose _Immutable_ and it was a real challenge for my pair
3.
  * Language: Rust
  * Paired with a mid-level embedded software developer (C)
  * Additional constraints: Evil TDD and/or no control-flow
  * We focused on the rover: making it move forward, teleport at boundaries, rotate
  * We picked both constraints, while _no control-flow_ was natural, _Evil TDD_ was really fun for me:
    * not because I have tried to trap my pair, on the contrary, I have tried to follow a classic TDD style, but at some point it was too uncomfortable for him and he went for a giant step
    * the fun part was in his effort to come with complex tests, while I came up with quick and simple implementations
4.
  * Language: Scala
  * Paired with a mid-level software developer and former part of my group I have already paired with a big number of times
  * Additional constraints: Test Commit or Revert and/or Ping-Pong
  * We focused on the rover: making it move forward, teleport at boundaries, rotate
  * We chose both constraints, but instead of the recommended 2 minutes for the _TCR_, I have used 4 minutes because I have not done any Scala for a long time, and I'm not used to Mac keyboard layout
5.
  * Language: TypeScript
  * Paired with a senior frontend software developer and part of my group I have already paired with a couple of times
  * Additional constraints: Random constraints (I have got "one level of indentation" and my pair "no control-flow"), but we have judged it was too easy, so we have added "no return"
  * We focused on the rover: making it move forward, teleport at boundaries, rotate
  * I think it was the most creative session, we have ended up with a map of lambdas for each command
6.
  * Language: F#
  * I was a mob programming with 10-12 other developers
  * Additional constraints: Evil TDD / Everything named with one letter
  * It was a fun session, we have started with the rover (again) and we went up to deal with obstacles as a simple list
  * It was interesting to see that even if our tests name were not helpful, it was key to understand the structure of our code
  * Also, we have seen that functions extractions did not help much for structuring code comprehension

## My feedback

Unlike last year, I did not have big expectations, consequently, I did not get
frustrated to achieve less and less, and I have had a lot of fun along the day.

I have given a ROTI of 4/5.

## My attempt

Here is my attempt, let's start with the Rover, without command:

```haskell
spec :: Spec
spec =
  describe "Mars Rover Kata" $ do
    it "No command should be at start" $
      roverCommands "" `shouldBe` "0:0:N"

roverCommands :: String -> String
roverCommands _ = "0:0:N"
```

It's hard-coded, let's write another one with a move:

```haskell
-- ...
it "Moving once should be in 0:1:N" $
  roverCommands "M" `shouldBe` "0:1:N"

roverCommands :: String -> String
roverCommands =
  \case
    "M" -> "0:1:N"
    _ -> "0:0:N"
```

Still enumerating, but we can refactor our tests:

```haskell
spec :: Spec
spec =
  describe "Mars Rover Kata" $ do
    forM_
      [ ("No command", "", "0:0:N"),
        ("Moving once", "M", "0:1:N")
      ]
      $ \(name, commands, result) ->
        it (name <> " should be " <> result) $
          roverCommands commands `shouldBe` result

roverCommands :: String -> String
roverCommands =
  \case
    "M" -> "0:1:N"
    _ -> "0:0:N"
```

Let's move without wrapping:

```haskell
("Moving four times (not wrapping)", "MMMM", "0:4:N")

roverCommands :: String -> String
roverCommands =
  \case
    xs -> "0:" <> show (length xs) <> ":N"
```

Let's move with wrapping:

```haskell
("Moving five times (wrapping)", "MMMMM", "0:0:N")

roverCommands :: String -> String
roverCommands =
  \case
    xs -> "0:" <> show (length xs `mod` 5) <> ":N"
```

We can rotate once:

```haskell
("Rotating Right", "R", "0:0:E")

roverCommands :: String -> String
roverCommands =
  \case
    "R" -> "0:0:E"
    xs -> "0:" <> show (length xs `mod` 5) <> ":N"
```

We can try rotating and moving forward:

```haskell
("Rotating Right and move", "RM", "1:0:E")

roverCommands :: String -> String
roverCommands = displayPosition . foldl' go (Rover {x = 0, y = 0, direction = North})
  where
    go p =
      \case
        'R' -> p {direction = East}
        _ ->
          case p.direction of
            North -> p {y = (p.y + 1) `mod` 5}
            East -> p {x = p.x + 1}

data Direction = North | East

data Rover = Rover
  { x :: Int,
    y :: Int,
    direction :: Direction
  }

displayPosition :: Rover -> String
displayPosition p = show p.x <> ":" <> show p.y <> ":" <> displayedPosition
  where
    displayedPosition =
      case p.direction of
        North -> "N"
        East -> "E"
```

That's a huge step, anyway, let's skip wrapping and other rotations:

```haskell
spec :: Spec
spec =
  describe "Mars Rover Kata" $ do
    forM_
      [ ("No command", "", "0:0:N"),
        ("Moving once", "M", "0:1:N"),
        ("Moving four times (not wrapping)", "MMMM", "0:4:N"),
        ("Moving five times (wrapping)", "MMMMM", "0:0:N"),
        ("Rotating Right", "R", "0:0:E"),
        ("Rotating Right and move", "RM", "1:0:E"),
        ("Rotating Right and move four times (not wrapping)", "RMMMM", "4:0:E"),
        ("Rotating Right and move five times (wrapping)", "RMMMMM", "0:0:E"),
        ("Rotating Left", "L", "0:0:W"),
        ("Rotating Left and move (wrapping)", "LM", "4:0:W"),
        ("Rotating Left and move five times (not wrapping twice)", "LMMMMM", "0:0:W"),
        ("Rotating Left and move six times (wrapping twice)", "LMMMMMM", "4:0:W"),
        ("Rotating Left twice", "LL", "0:0:S"),
        ("Rotating Left twice and move (wrapping)", "LLM", "0:4:S"),
        ("Rotating Left twice and move five times (not wrapping twice)", "LLMMMMM", "0:0:S"),
        ("Rotating Left twice and move six times (wrapping twice)", "LLMMMMMM", "0:4:S"),
        ("Rotating Right twice", "RR", "0:0:S"),
        ("Rotating Right three times", "RRR", "0:0:W")
      ]
      $ \(name, commands, result) ->
        it (name <> " should be " <> result) $
          roverCommands commands `shouldBe` result

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
        _ ->
          case p.direction of
            North -> p {y = (p.y + 1) `mod` 5}
            East -> p {x = (p.x + 1) `mod` 5}
            South -> p {y = (p.y - 1) `mod` 5}
            West -> p {x = (p.x - 1) `mod` 5}

data Direction = North | East | South | West

data Rover = Rover
  { x :: Int,
    y :: Int,
    direction :: Direction
  }

displayPosition :: Rover -> String
displayPosition p = show p.x <> ":" <> show p.y <> ":" <> displayedPosition
  where
    displayedPosition =
      case p.direction of
        North -> "N"
        East -> "E"
        South -> "S"
        West -> "W"
```

Finally, we can deal with obstacle:

```haskell
    -- ...
        ("Blocked rover have to rotate", [Position 0 1], "MRM", "1:0:E")
      ]
      $ \(name, obstacles, commands, result) ->
        it (name <> " should be " <> result) $
          roverCommands obstacles commands `shouldBe` result

roverCommands :: [Position] -> String -> String
roverCommands obstacles =
  displayPosition . foldl' go (Rover {position = Position {x = 0, y = 0}, direction = North})
  where
    go p =
      \case
        -- ...
        'M' ->
          let nextRover =
                case p.direction of
                  North -> p {position = (p.position) {y = (p.position.y + 1) `mod` 5}}
                  East -> p {position = (p.position) {x = (p.position.x + 1) `mod` 5}}
                  South -> p {position = (p.position) {y = (p.position.y - 1) `mod` 5}}
                  West -> p {position = (p.position) {x = (p.position.x - 1) `mod` 5}}
           in if notElem nextRover.position obstacles
                then nextRover
                else p
        c -> error $ "Unknown command " <> show c

data Rover = Rover
  { position :: Position,
    direction :: Direction
  }

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving stock (Eq)
```

I had to extract `Position` from `Rover`, that's why I never start with data
types when I design, because they are bound to my functions (and the system's flow).

I could create `Grid` and delegate it new position computation:

```haskell
      -- ...
      $ \(name, obstacles, commands, result) ->
        it (name <> " should be " <> result) $
          roverCommands (computeNextPosition $ Grid obstacles) commands `shouldBe` result

roverCommands :: (Position -> Maybe Position) -> String -> String
roverCommands nextPosition =
        -- ...
        'M' ->
          let potentialPosition =
                case p.direction of
                  North -> (p.position) {y = p.position.y + 1}
                  East -> (p.position) {x = p.position.x + 1}
                  South -> (p.position) {y = p.position.y - 1}
                  West -> (p.position) {x = p.position.x - 1}
           in p {position = fromMaybe p.position $ nextPosition potentialPosition}
        c -> error $ "Unknown command " <> show c

newtype Grid = Grid {obstacles :: [Position]}

computeNextPosition :: Grid -> Position -> Maybe Position
computeNextPosition grid p =
  if notElem normalizedPosition grid.obstacles
    then Just normalizedPosition
    else Nothing
  where
    normalizedPosition = Position {x = p.x `mod` 5, y = p.y `mod` 5}
```
