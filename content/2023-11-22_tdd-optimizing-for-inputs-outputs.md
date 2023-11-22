+++
title = "TDD: optimizing for inputs vs for outputs"
date = 2022-11-22
draft = false
path = "2023-11/tdd-optimizing-for-inputs-outputs"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo"]
+++

There is a famous code kata used during [Global Days of Code Retreat](https://www.coderetreat.org/)
called [Conway's Game Of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

It has four rules:

1. Any live cell with fewer than two live neighbours dies, as if by underpopulation
2. Any live cell with two or three live neighbours lives on to the next generation
3. Any live cell with more than three live neighbours dies, as if by overpopulation
4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction

I have a love - hate relationship with this code kata.

On the positive side:

* It was one of the first code kata I have seriously worked on (especially during my first GDCR in 2012)
* It is a simple and yet challenging kata
* It shaped my TDD style (inside-out)

On the negative side:

* I have overdone it (more than 120 times)
* When I do it, I tend to not be patient

I'll use it to illustrate my point, which is: there are two ways to optimize
a piece of code in TDD, for outputs or for inputs.

Let's start with outputs optimizations:

I always start by implementing the rule 4, focusing on the cell (not the grid):

```haskell
spec :: Spec
spec = do
    describe "Game of life" $ do
      it "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction" $
        nextGen Dead 3 `shouldBe` Alive

data Cell 
  = Alive
  | Dead
  deriving stock (Eq, Show)

newtype Neighbours 
  = Neighbours { getNeighbours :: Int }
  deriving newtype (Eq, Ord, Show, Num)

nextGen :: Cell -> Neighbours -> Cell
nextGen _ _ = Alive
```

Then I implement rule 3:

```haskell
-- ...
it "Any live cell with more than three live neighbours dies, as if by overpopulation" $
  nextGen Alive 4 `shouldBe` Dead
-- ...

nextGen :: Cell -> Neighbours -> Cell
nextGen _ =
  \case
    3 -> Alive
    _ -> Dead
```

```haskell
-- ...
it "Any live cell with two live neighbours lives on to the next generation" $
  nextGen Alive 2 `shouldBe` Alive
it "Any live cell with three live neighbours lives on to the next generation" $
  nextGen Alive 3 `shouldBe` Alive
-- ...

nextGen :: Cell -> Neighbours -> Cell
nextGen _ =
  \case
    2 -> Alive
    3 -> Alive
    _ -> Dead
```

At this point, my implementation is incorrect as a dead cell with two neighbours
or less should stay dead.

Moreover, in a pure TDD-style, I won't be able to add tests not passing to cover
rule 1, so I add few "business tests:"

```haskell
-- ...
it "Any Dead cell with fewer than three (2) live neighbours stays dead on to the next generation" $
  nextGen Dead 2 `shouldBe` Dead
it "Any live cell with fewer than two (1) live neighbours dies, as if by underpopulation" $
  nextGen Alive 1 `shouldBe` Dead
it "Any live cell with fewer than two (0) live neighbours dies, as if by underpopulation" $
  nextGen Alive 0 `shouldBe` Dead
-- ...

nextGen :: Cell -> Neighbours -> Cell
nextGen x =
  \case
    2 -> x
    3 -> Alive
    _ -> Dead
```

That's what I call optimizing for outputs: implementation does not matter, as
long as you have the correct values, the production code can mask the business
side (which are in the tests).

On another hand, there are what I call optimizing for inputs:

Let's start with rule 4:

```haskell
-- ...
describe "Reproduction (three live neighbours)" $ do
  it "Any dead cell with exactly three live neighbours becomes a live cell" $
    reproduction.next Dead `shouldBe` Alive
  it "Any live cell with three live neighbours lives on to the next generation" $
    reproduction.next Alive `shouldBe` Alive
-- ...

newtype Neighbourhood
  = Neighbourhood { next :: Cell -> Cell }

reproduction :: Neighbourhood
reproduction = Neighbourhood $ const Alive
```

Then rule 3:

```haskell
-- ...
describe "Overpopulation (more than three live neighbours)" $ do
  it "Any live cell with more than three live neighbours dies" $
    overpopulation.next Alive `shouldBe` Dead
-- ...

overpopulation :: Neighbourhood
overpopulation = Neighbourhood $ const Dead
```

Then rule 2:

```haskell
-- ...
describe "Survive (two live neighbours)" $ do
  it "Any Dead cell with fewer than three live neighbours stays dead on to the next generation" $
    survive.next Dead `shouldBe` Dead
  it "Any dead cell with exactly three live neighbours becomes a live cell" $
    survive.next Alive `shouldBe` Alive
-- ...

survive :: Neighbourhood
survive = Neighbourhood id
```

And finally rule 1:

```haskell
-- ...
describe "Underpopulation (zero or one live neighbours)" $ do
  it "Any Dead cell with fewer than three live neighbours stays dead on to the next generation" $
    underpopulation.next Dead `shouldBe` Dead
  it "Any live cell with fewer than two live neighbours dies" $
    underpopulation.next Alive `shouldBe` Dead
-- ...

underpopulation :: Neighbourhood
underpopulation = Neighbourhood $ const Dead
```

Of course, it takes an effort to talk with the business department to structure
the problem's understanding, but at least each rule is clearly defined in the
production code, not only in tests.

Moreover, each rule is independent.

Bonus, that's the only approach I know to have an implementation without
condition/patter-matching, but that's the subject of another log.
