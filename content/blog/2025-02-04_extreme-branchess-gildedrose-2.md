+++
title = "Extreme branchless: Gilded Rose Part 2"
date = 2025-02-04
draft = false
path = "2025-02/extreme-branchless-gildedrose-2"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [attempt](@/blog/2025-01-21_extreme-branchess-gildedrose-1.md) of
the [Gilded Rose](https://codingdojo.org/kata/gilded-rose/).

We can start to inline and simplify `mkBaseCase`:

```haskell
agedBrie :: Int -> Int -> Item'
agedBrie sellIn quality =
  Item'
    { _name = "Aged Brie",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        agedBrie (sellIn - 1) . min 50 $
          if sellIn < 1
          then quality + 2
          else quality + 1
    }

backstagePasses :: Int -> Int -> Item'
backstagePasses sellIn quality =
  Item'
    { _name = "Backstage passes to a TAFKAL80ETC concert",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        let
          quality'
            | sellIn < 6 = min 50 (quality + 3)
            | sellIn < 11 = min 50 (quality + 2)
            | otherwise = min 50 (quality + 1)
        in
          backstagePasses (sellIn - 1) $
             if sellIn < 1
             then 0
             else quality'
    }

sulfuras :: Int -> Int -> Item'
sulfuras sellIn quality =
  Item'
    { _name = "Sulfuras, Hand of Ragnaros",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality = sulfuras sellIn quality
    }

baseCase :: String -> Int -> Int -> Item'
baseCase name sellIn quality =
  Item'
    { _name = name,
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        baseCase name (sellIn - 1) . max 0 $
          if quality > 1 && sellIn < 1
          then quality - 2
          else quality - 1
    }
```

Great so far, each case is well-defined and has no dependency.

Our next step is to get rid of `if`, we can rely on (infinite) lists to map values:

```haskell
agedBrie :: Int -> Int -> Item'
agedBrie sellIn quality =
  Item'
    { _name = "Aged Brie",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        agedBrie (sellIn - 1) . min 50 $
          ((quality + 2) : repeat (quality + 1)) `at` sellIn
    }
```

Note: `at` is like `(!!)`, except that it does not throw when the index is negative.

Here we build a list corresponding to the `if` statement:

* `quality + 2` when `sellIn == 0` (`sellIn < 1`), there's no difference in this design
* `quality + 1` otherwise

The next one works in the same way:

```haskell
backstagePasses :: Int -> Int -> Item'
backstagePasses sellIn quality =
  Item'
    { _name = "Backstage passes to a TAFKAL80ETC concert",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        let
          quality' =
              min 50 $
                (replicate 6 (quality + 3) <> replicate 5 (quality + 2) <> repeat (quality + 1)) `at` sellIn
        in
          backstagePasses (sellIn - 1) $
            (0 : repeat quality') `at` sellIn
    }
```

The next one is a bit trickier since it is based on two properties
`sellIn` and `quality`.

We can rely on shortcuts with `Maybe a`, for instance, it comes with many
functions, such as `(<*)` which works as follows:

* `Nothing <* Nothing == Nothing`
* `Just 1 <* Nothing == Nothing`
* `Nothing <* Just 1 == Nothing`
* `Just 1 <* Just () == Just 1`

So, we have to design a value space for each property and join them:

```haskell
baseCase :: String -> Int -> Int -> Item'
baseCase name sellIn quality =
  Item'
    { _name = name,
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        baseCase name (sellIn - 1) . max 0 . fromMaybe (quality - 1) $
          (Just (quality - 2) : repeat Nothing) `at` sellIn <* (Nothing : Nothing : repeat (Just ())) `at` quality
    }
```

We have a value when `sellIn == 0 && quality >= 2`, we index each property
on its value space, join them with `(<*)`, if they have both a `Just _`, it
means their conditions are met, and we return `quality + 1`, or we default on `quality - 1`.

And that's it, a pure branchless Gilded Rose.
