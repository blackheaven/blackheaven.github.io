+++
title = "Extreme branchless: Bowling"
date = 2024-12-17
draft = false
path = "2024-12/extreme-branchless-bowling"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey,

Let's [refactor](@/blog/2024-09-17_bowling-zipper.md) the [bowling kata](https://codingdojo.org/kata/Bowling/).
As a reminder, we had this implementation:

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

data BowlingZipper a = Z a a [a]
```

We have several branching spots:

* `go` is the worst as it contains many checks on integers
* `unfoldr` which creates a list, consumed directly

Unlike our previous attempts, we can't simply count the number of pins down,
describe a strategy, apply the behavior, as we have to either count one or two pins.

Instead, we'll have to reuse a technique introduced for [FizzBuzz](@/blog/2024-03-13_fizzbuzz.md):
compute every possible solution, prioritizing the most relevant.

Which means, we'll list our strategies, giving the value for both a first frame and the sum of two frames:

```haskell
data PinsDown = PinsDown
  { firstFrame :: BowlingZipper Natural -> Maybe (Natural, BowlingZipper Natural)
  , bothFrames :: BowlingZipper Natural -> Maybe (Natural, BowlingZipper Natural)
  , next :: PinsDown
  }
```

Then, we can list our strategies:

```haskell
pins0, pins1, pins2, pins3, pins4, pins5, pins6, pins7, pins8, pins9, pins10 :: PinsDown
pins0 = pinOthers pins1
pins1 = pinOthers pins2
pins2 = pinOthers pins3
pins3 = pinOthers pins4
pins4 = pinOthers pins5
pins5 = pinOthers pins6
pins6 = pinOthers pins7
pins7 = pinOthers pins8
pins8 = pinOthers pins9
pins9 = pinOthers pins10
pins10 =
  PinsDown
    { firstFrame = \(Z x y (z : nexts)) -> Just (10 + y + z, Z y z nexts)
    , bothFrames = \(Z x y (z : nexts)) -> Just (10 + z, toBowlingZipper (z : nexts))
    , next = pins10
    }

pinOthers :: PinsDown -> PinsDown
pinOthers nextPinsDown =
  PinsDown
    { firstFrame = const Nothing
    , bothFrames = const Nothing
    , next = nextPinsDown
    }
```

Note:

* Only we only have a special behavior when all pins are down, in all other scenarios, we only sum pins
* `toBowlingZipper` has been extracted as follows, even though it's a partial function, only working with our inputs' structure, it's fine in our use-case, in real-world systems, I would protect (not export) the function and strong-type the input.

```haskell
toBowlingZipper :: [a] -> BowlingZipper a
toBowlingZipper (x : y : nexts) = Z x y nexts
```

Finally, we can rewrite our main function:

```haskell
bowlingScore :: [Natural] -> Natural
bowlingScore = sum . take 10 . unfoldr (Just . go) . toBowlingZipper
  where
    go zipper@(Z x y nexts) =
      fromMaybe (x + y, toBowlingZipper nexts) $
        (strategyFor x).firstFrame zipper <|> (strategyFor (x + y)).bothFrames zipper
    strategyFor (Natural f) = f (.next) pins0
```

Note:

* `strategyFor` pick the relevant strategy, as we've seen it few time in previous logs
* Then we pick the result as follows:
  + Pick the strategy for the first frame, look at the strategy result for the first frame
  + Otherwise, pick the strategy for both frames summed, look at the strategy result for both frames
  + Otherwise, it simply sums the two first frames

It's a good first strep, we could go further, rebuilding "low-level" abstraction
to `unfoldr`/`take`/`sum`, but let's assume we use branchless implementations.
