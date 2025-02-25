+++
title = "Extreme branchless: Minesweeper"
date = 2025-02-25
draft = false
path = "2025-02/extreme-branchless-minesweeper"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

This time: [Minesweeper](https://codingdojo.org/kata/Minesweeper/) kata.

I used to play to Minesweeper a lot during my childhood, the kata works as follows:

* Each cell can have a mine
* If the cell has a mine, display `*`
* If the cell has no mine, display the number of cells in the surrounding cells (up, down, left, right, and diagonals)

Let's set up a basic structure:

```haskell
spec :: Spec
spec =
  describe "Minesweeper" $ do
    it "an empty grid should return an empty grid" $
      minesweeper [[]] `shouldBe` [[]]

data Cell

minesweeper :: [[Cell]] -> [[Char]]
minesweeper _ = [[]]
```

Then an empty cell:

```haskell
spec :: Spec
spec =
  describe "Minesweeper" $ do
    let e = emptyCell
    it "an empty grid should return an empty grid" $
      minesweeper [[]] `shouldBe` [[]]
    it "a grid without mines should return only 0" $
      minesweeper [[e]] `shouldBe` [['0']]

newtype Cell = Cell { unCell :: Char }

emptyCell :: Cell
emptyCell = Cell '0'

minesweeper :: [[Cell]] -> [[Char]]
minesweeper = map $ map unCell
```

We can introduce mines:

```haskell
-- it "a grid with only a mine should return only *" $
--   minesweeper [[m]] `shouldBe` [['*']]

mine :: Cell
mine = Cell '*'

minesweeper :: [[Cell]] -> [[Char]]
minesweeper = map $ map unCell
```

Not a big change.

Now, things are getting more complex, we want to count the number of mines.

We have to:

* Add the mine count on `Cell`
* Sum the surrounding count

```haskell
-- it "a grid with a mine and a right empty should return *1" $
--   minesweeper [[m, e]] `shouldBe` [['*', '1']]

data Cell = Cell
  { displayCell :: Int -> Char
  , minesCount :: Int
  }

emptyCell :: Cell
emptyCell =
  Cell
    { displayCell = head . show
    , minesCount = 0
    }

mine :: Cell
mine =
  Cell
    { displayCell = const '*'
    , minesCount = 1
    }

minesweeper :: [[Cell]] -> [[Char]]
minesweeper original = zipWith (zipWith (.displayCell)) original counts
  where counts = zipWith (zipWith (+)) originalCount countSl1
        originalCount = map (.minesCount) <$> original
        countSl1 = (0 :) <$> originalCount
```

Here, we have tested the mine count on a right empty cell, to do so, we rely on
`zipWith` behavior which crop the list on the sorthest one.
We have added an element (`0`), at the top of the list, shifting counts and
effectively fetching left count.

Let's do the same on a left empty cell:

```haskell
-- it "a grid with a mine and a left empty should return 1*" $
--   minesweeper [[e, m]] `shouldBe` [['1', '*']]

minesweeper :: [[Cell]] -> [[Char]]
minesweeper original = zipWith (zipWith (.displayCell)) original (tail <$> counts)
  where counts = zipWith3 (zipWith3 (\a b c -> a + b + c)) originalCount countSr1 countSl1
        originalCount = (0 :) . map (.minesCount) <$> original
        countSl1 = (0 :) <$> originalCount
        countSr1 = (<> [0]) . tail <$> originalCount
```

This is mostly the same tactics, except that we drop the front element and add
a `0`-item at the end.

Then, we want to handle vertical cells, to do so, the simplest approach is to
transpose the grid (i.e. inverting columns and rows), and apply our shifting
operations:

```haskell
-- it "a grid with a mine and a top, bottom empty should return [1, *, 1]" $
--   minesweeper [[e], [m], [e]] `shouldBe` [['1'], ['*'], ['1']]

minesweeper :: [[Cell]] -> [[Char]]
minesweeper [[]] = [[]]
minesweeper original = zipWith (zipWith (.displayCell)) original counts
  where counts =
          zipWith3
            (zipWith3 (\a b c -> a + b + c))
            originalCount
            (counting originalCount)
            (transpose $ counting $ transpose originalCount)
        originalCount = map (.minesCount) <$> original
        counting xs = zipWith (zipWith (+)) (countSr xs) (countSl xs)
        countSl = map (0 :)
        countSr = map $ (<> [0]) . tail
```

Diagonals are actually simpler to handle, instead of shifting on each line,
we have to add a full row at the top, or drop the top one and add one at the
bottom:

```haskell
-- it "a grid with a mine and bordered with empty cells should return [111, 1*1, 111]" $
--   minesweeper [[e, e, e], [e, m, e], [e, e, e]] `shouldBe` ["111", "1*1", "111"]

minesweeper :: [[Cell]] -> [[Char]]
minesweeper [[]] = [[]]
minesweeper original = zipWith (zipWith (.displayCell)) original counts
  where counts =
          zipWith5
            (zipWith5 (\a b c d e -> a + b + c + d + e))
            originalCount
            (counting originalCount)
            (transpose $ counting $ transpose originalCount)
            (counting $ repeat 0 : originalCount)
            (counting $ tail originalCount <> [repeat 0])
        originalCount = map (.minesCount) <$> original
        counting xs = zipWith (zipWith (+)) (countSr xs) (countSl xs)
        countSl = map (0 :)
        countSr = map $ (<> [0]) . tail
```

Let's add a more complex grid, just for fun:

```haskell
it "a grid with a mine on each corner should return [*2*, 242, *2*]" $
  minesweeper [[m, e, m], [e, e, e], [m, e, m]] `shouldBe` ["*2*", "242", "*2*"]
```

Here we are!
