+++
title = "Bowling kata as a zipper"
date = 2024-09-17
draft = false
path = "2024-09/bowling-zipper"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo"]
+++

A few days ago I have practiced the [bowling kata](https://codingdojo.org/kata/Bowling/)
with one of the members of the [Software Crafters Lyon](https://swcraftlyon.github.io/).

It's a kata I've only tried two times before that (and I've never played a real
world bowling game, though, there is a bowling 10 minutes away from my home).

Let's dive in with some basic tests:

```haskell
it "A game with no pins down should be 0" $
  bowlingScore (replicate 20 0) `shouldBe` 0
it "A game with two pins down on each try should be 40" $
  bowlingScore (replicate 20 2) `shouldBe` 40
```

Note: I have chosen to represent tries and not frames for simplicity, which
is not a safe representation, but we aim to trust our inputs.

A trivial implementation is to simply sum tries:

```haskell
bowlingScore :: [Int] -> Int
bowlingScore = sum
```

Then, we should handle spares (when it takes two tries to knock them all down):

```haskell
it "A game with all 5-spares frames should be 150" $
  bowlingScore (replicate 21 5) `shouldBe` 150
```

In order to do that show somehow model the frame.

An easy way to have arbitrary list traversal is to use `unfoldr` which iterate
while having a valid output:

```haskell
bowlingScore :: [Int] -> Int
bowlingScore = sum . take 10 . unfoldr go
  where
    go =
      \case
        (x : y : z : nexts)
          | x + y == 10 -> Just (10 + z, z : nexts)
          | otherwise -> Just (x + y, z : nexts)
        (x : y : nexts) -> Just (x + y, nexts)
        [] -> Nothing
```

There is a design tension here:

* `go` is partial (i.e. we do not handle 1-element list `[_]`), in production code, warning would make it fail
* `take 10` emphasis the number of frames

Finally we have to handle strikes (10 pins down in one try):

```haskell
it "A game with all strike frames should be 300" $
  bowlingScore (replicate 12 10) `shouldBe` 300
```

All we have to do it add one line for this case:

```haskell
bowlingScore :: [Int] -> Int
bowlingScore = sum . take 10 . unfoldr go
  where
    go =
      \case
        (x : y : z : nexts)
          | x == 10 -> Just (10 + y + z, y : z : nexts)
          | x + y == 10 -> Just (10 + z, z : nexts)
          | otherwise -> Just (x + y, z : nexts)
        (x : y : nexts) -> Just (x + y, nexts)
        [] -> Nothing
```

There is something quite annoying: in each of our cases we look for two or
three elements which looks like a good use case for a [Zipper](https://wiki.haskell.org/Zipper).

It could be as simple as:

```haskell
data BowlingZipper a = Z a a [a]
```

We can rewrite our function adding a local [smart constructor](https://wiki.haskell.org/Smart_constructors):

```haskell
bowlingScore :: [Int] -> Int
bowlingScore = sum . take 10 . unfoldr go . toBowlingZipper
  where
    go =
      \case
        Z x y (z : nexts)
          | x == 10 -> Just (10 + y + z, Z y z nexts)
          | x + y == 10 -> Just (10 + z, toBowlingZipper (z : nexts))
          | otherwise -> Just (x + y, toBowlingZipper (z : nexts))
        Z x y nexts -> Just (x + y, toBowlingZipper nexts)
    toBowlingZipper (x : y : nexts) = Z x y nexts
```

`go` is now a total function (i.e. all branches are covered), only
`toBowlingZipper` (the smart constructor) is partial, which is better, we could
push it to the boundaries in more complex project, letting our core domain work
only with sound values.
