+++
title = "Extreme branchless: Gilded Rose Part 1"
date = 2025-01-21
draft = false
path = "2025-01/extreme-branchless-gildedrose-1"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey,
today, I'd like to have a look at [Gilded Rose](https://codingdojo.org/kata/gilded-rose/).

This is a classical legacy/refactoring kata, I didn't like it, but this time, I enjoyed it.

Unlike the [original code](https://github.com/emilybache/GildedRose-Refactoring-Kata),
we'll focus on the evolution of one item:

```haskell
data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: Item -> Item
updateQuality (Item name sellIn quality) =
  let
    quality' =
      if name /= "Aged Brie"
         && name /= "Backstage passes to a TAFKAL80ETC concert"
      then
        if quality > 0
        then
          if name /= "Sulfuras, Hand of Ragnaros"
          then quality - 1
          else quality
        else quality
      else
        if quality < 50
        then
          quality + 1 +
            (if name == "Backstage passes to a TAFKAL80ETC concert"
             then
               if sellIn < 11
               then
                 if quality < 49
                 then
                   1 + (if sellIn < 6
                        then
                          if quality < 48
                          then 1
                          else 0
                        else 0)
                 else 0
               else 0
             else 0)
        else quality

    sellIn' =
      if name /= "Sulfuras, Hand of Ragnaros"
      then sellIn - 1
      else sellIn
  in
    if sellIn' < 0
    then
      if name /= "Aged Brie"
      then
        if name /= "Backstage passes to a TAFKAL80ETC concert"
        then
          if quality' > 0
          then
            if name /= "Sulfuras, Hand of Ragnaros"
            then (Item name sellIn' (quality' - 1))
            else (Item name sellIn' quality')
          else (Item name sellIn' quality')
        else (Item name sellIn' (quality' - quality'))
      else
        if quality' < 50
        then (Item name sellIn' (quality' + 1))
        else (Item name sellIn' quality')
    else (Item name sellIn' quality')
```

No so good.

Let's add some tests:

```haskell
spec :: Spec
spec =
  describe "Kata" $ do
    let comparedItems :: [(Item, [(Int, Item, Item)])]
        comparedItems =
          flip map initialInventory $ \item ->
            (item, zip3 [0..100] (iterate updateQuality item) (iterate updateQualityLegacy item))
    forM_ comparedItems $ \(initialItem, dailyitems) ->
      describe (show initialItem) $
        forM_ dailyitems $ \(day, actualItem, expectedItem) ->
          it ("Day " <> show day) $
            actualItem `shouldBe` expectedItem

initialInventory :: [Item]
initialInventory =
  [ Item "+5 Dexterity Vest"                          10  20
  , Item "Aged Brie"                                   2   0
  , Item "Elixir of the Mongoose"                      5   7
  , Item "Sulfuras, Hand of Ragnaros"                  0  80
  , Item "Sulfuras, Hand of Ragnaros"                (-1) 80
  , Item "Backstage passes to a TAFKAL80ETC concert"  15  20
  , Item "Backstage passes to a TAFKAL80ETC concert"  10  49
  , Item "Backstage passes to a TAFKAL80ETC concert"   5  49
  -- this conjured item does not work properly yet
  , Item "Conjured Mana Cake"                          3   6
  ]
```

Let's review the tests strategy:

* We have one `decribe`per `initialItem`
* We have one `it` per `day` (101, from 0 to 100)
* For each day, we compare, side by side, in an assertion (`shouldBe`), current, refactored version and the legacy version (duplicated) for the item update `day` times
* It gives us 909 tests, good enough

Let's start slowly:
* Swap `let`/`in` to `where`
* Drop extra parenthesis

```haskell
updateQuality :: Item -> Item
updateQuality (Item name sellIn quality) =
    if sellIn' < 0
    then
      if name /= "Aged Brie"
      then
        if name /= "Backstage passes to a TAFKAL80ETC concert"
        then
          if quality' > 0
          then
            if name /= "Sulfuras, Hand of Ragnaros"
            then (Item name sellIn' (quality' - 1))
            else (Item name sellIn' quality')
          else (Item name sellIn' quality')
        else (Item name sellIn' (quality' - quality'))
      else
        if quality' < 50
        then (Item name sellIn' (quality' + 1))
        else (Item name sellIn' quality')
    else (Item name sellIn' quality')
  where
    quality' =
      if name /= "Aged Brie"
         && name /= "Backstage passes to a TAFKAL80ETC concert"
      then
        if quality > 0
        then
          if name /= "Sulfuras, Hand of Ragnaros"
          then quality - 1
          else quality
        else quality
      else
        if quality < 50
        then
          quality + 1 +
            (if name == "Backstage passes to a TAFKAL80ETC concert"
             then
               if sellIn < 11
               then
                 if quality < 49
                 then
                   1 + (if sellIn < 6
                        then
                          if quality < 48
                          then 1
                          else 0
                        else 0)
                 else 0
               else 0
             else 0)
        else quality
```

Nothing exceptional.

Then, I have followed all `hlint` lints:

```haskell
    if sellIn' < 0
    then
      if name /= "Aged Brie"
      then
        if name /= "Backstage passes to a TAFKAL80ETC concert"
        then
          if (quality' > 0) && (name /= "Sulfuras, Hand of Ragnaros")
          then Item name sellIn' (quality' - 1)
          else Item name sellIn' quality'
        else Item name sellIn' (quality' - quality')
      else
        if quality' < 50
        then Item name sellIn' (quality' + 1)
        else Item name sellIn' quality'
    else Item name sellIn' quality'
  where
    quality'
      | name /= "Aged Brie"
         && name /= "Backstage passes to a TAFKAL80ETC concert" =
         if (quality > 0) && (name /= "Sulfuras, Hand of Ragnaros")
         then quality - 1
         else quality
      | quality < 50 = quality + 1 +
            (if ((name == "Backstage passes to a TAFKAL80ETC concert") && (sellIn < 11)) && (quality < 49)
             then 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0) else 0)
      | otherwise = quality

    sellIn' =
      if name /= "Sulfuras, Hand of Ragnaros"
      then sellIn - 1
      else sellIn

```

It starts to be less nested, let's keep going that way, focusing on `name` check
and guards:

```haskell
updateQuality :: Item -> Item
updateQuality (Item name sellIn quality)
  | name == "Aged Brie" && sellIn' < 0 =
      if quality' < 50
      then Item name sellIn' (quality' + 1)
      else Item name sellIn' quality'
  | name == "Backstage passes to a TAFKAL80ETC concert" && sellIn' < 0 =
      Item name sellIn' 0
  | name == "Sulfuras, Hand of Ragnaros" && sellIn' < 0 =
      Item name sellIn' quality'
  | otherwise =
      if quality' > 0 && sellIn' < 0
      then Item name sellIn' (quality' - 1)
      else Item name sellIn' quality'
  where
    quality'
      | name == "Backstage passes to a TAFKAL80ETC concert" =
        if sellIn < 11 && quality < 49
        then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
        else
          if quality < 50 then quality + 1
          else quality
      | name == "Sulfuras, Hand of Ragnaros" =
         quality
      | name /= "Aged Brie" =
         if quality > 0
         then quality - 1
         else quality
      | quality < 50 = quality + 1
      | otherwise = quality

    sellIn' =
      if name == "Sulfuras, Hand of Ragnaros"
      then sellIn
      else sellIn - 1
```

Then, we want to focus only on `name`, to do so, we ha to nest `sellIn'`:

```haskell
updateQuality (Item name sellIn quality)
  | name == "Aged Brie" =
     if sellIn' < 0
     then
       if quality' < 50
       then Item name sellIn' (quality' + 1)
       else Item name sellIn' quality'
     else
       if quality' > 0 && sellIn' < 0
       then Item name sellIn' (quality' - 1)
       else Item name sellIn' quality'
  | name == "Backstage passes to a TAFKAL80ETC concert" =
     if sellIn' < 0
     then
       Item name sellIn' 0
     else
       if quality' > 0 && sellIn' < 0
       then Item name sellIn' (quality' - 1)
       else Item name sellIn' quality'
  | name == "Sulfuras, Hand of Ragnaros" =
     if sellIn' < 0
     then
       Item name sellIn' quality'
     else
       if quality' > 0 && sellIn' < 0
       then Item name sellIn' (quality' - 1)
       else Item name sellIn' quality'
  | otherwise =
      if quality' > 0 && sellIn' < 0
      then Item name sellIn' (quality' - 1)
      else Item name sellIn' quality'
  where
    quality'
      | name == "Backstage passes to a TAFKAL80ETC concert" =
        if sellIn < 11 && quality < 49
        then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
        else
          if quality < 50 then quality + 1
          else quality
      | name == "Sulfuras, Hand of Ragnaros" =
         quality
      | name /= "Aged Brie" =
         if quality > 0
         then quality - 1
         else quality
      | quality < 50 = quality + 1
      | otherwise = quality

    sellIn' =
      if name == "Sulfuras, Hand of Ragnaros"
      then sellIn
      else sellIn - 1
```

In order to isolate more and more use-cases, we can inline `sellIn'`/`quality'`:

```haskell
updateQuality :: Item -> Item
updateQuality (Item name sellIn quality)
  | name == "Aged Brie" =
    let
      quality'
        | name == "Backstage passes to a TAFKAL80ETC concert" =
          if sellIn < 11 && quality < 49
          then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
          else
            if quality < 50 then quality + 1
            else quality
        | name == "Sulfuras, Hand of Ragnaros" =
           quality
        | name /= "Aged Brie" =
           if quality > 0
           then quality - 1
           else quality
        | quality < 50 = quality + 1
        | otherwise = quality

      sellIn' =
        if name == "Sulfuras, Hand of Ragnaros"
        then sellIn
        else sellIn - 1
    in
       if sellIn' < 0
       then
         if quality' < 50
         then Item name sellIn' (quality' + 1)
         else Item name sellIn' quality'
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | name == "Backstage passes to a TAFKAL80ETC concert" =
    let
      quality'
        | name == "Backstage passes to a TAFKAL80ETC concert" =
          if sellIn < 11 && quality < 49
          then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
          else
            if quality < 50 then quality + 1
            else quality
        | name == "Sulfuras, Hand of Ragnaros" =
           quality
        | name /= "Aged Brie" =
           if quality > 0
           then quality - 1
           else quality
        | quality < 50 = quality + 1
        | otherwise = quality

      sellIn' =
        if name == "Sulfuras, Hand of Ragnaros"
        then sellIn
        else sellIn - 1
    in
       if sellIn' < 0
       then
         Item name sellIn' 0
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | name == "Sulfuras, Hand of Ragnaros" =
    let
      quality'
        | name == "Backstage passes to a TAFKAL80ETC concert" =
          if sellIn < 11 && quality < 49
          then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
          else
            if quality < 50 then quality + 1
            else quality
        | name == "Sulfuras, Hand of Ragnaros" =
           quality
        | name /= "Aged Brie" =
           if quality > 0
           then quality - 1
           else quality
        | quality < 50 = quality + 1
        | otherwise = quality

      sellIn' =
        if name == "Sulfuras, Hand of Ragnaros"
        then sellIn
        else sellIn - 1
    in
       if sellIn' < 0
       then
         Item name sellIn' quality'
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | otherwise =
    let
      quality'
        | name == "Backstage passes to a TAFKAL80ETC concert" =
          if sellIn < 11 && quality < 49
          then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
          else
            if quality < 50 then quality + 1
            else quality
        | name == "Sulfuras, Hand of Ragnaros" =
           quality
        | name /= "Aged Brie" =
           if quality > 0
           then quality - 1
           else quality
        | quality < 50 = quality + 1
        | otherwise = quality

      sellIn' =
        if name == "Sulfuras, Hand of Ragnaros"
        then sellIn
        else sellIn - 1
    in
        if quality' > 0 && sellIn' < 0
        then Item name sellIn' (quality' - 1)
        else Item name sellIn' quality'
```

This is even bigger than the legacy version.

Simplifying `quality'`/`sellin'` gets us to:

```haskell
updateQuality :: Item -> Item
updateQuality (Item name sellIn quality)
  | name == "Aged Brie" =
    let
      quality'
        | quality < 50 = quality + 1
        | otherwise = quality

      sellIn' =
        sellIn - 1
    in
       if sellIn' < 0
       then
         if quality' < 50
         then Item name sellIn' (quality' + 1)
         else Item name sellIn' quality'
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | name == "Backstage passes to a TAFKAL80ETC concert" =
    let
      quality' =
          if sellIn < 11 && quality < 49
          then quality + 1 + 1 + (if (sellIn < 6) && (quality < 48) then 1 else 0)
          else
            if quality < 50 then quality + 1
            else quality

      sellIn' =
        sellIn - 1
    in
       if sellIn' < 0
       then
         Item name sellIn' 0
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | name == "Sulfuras, Hand of Ragnaros" =
    let
      quality' =
        quality

      sellIn' =
        sellIn
    in
       if sellIn' < 0
       then
         Item name sellIn' quality'
       else
         if quality' > 0 && sellIn' < 0
         then Item name sellIn' (quality' - 1)
         else Item name sellIn' quality'
  | otherwise =
    let
      quality'=
         if quality > 0
         then quality - 1
         else quality

      sellIn' =
        sellIn - 1
    in
        if quality' > 0 && sellIn' < 0
        then Item name sellIn' (quality' - 1)
        else Item name sellIn' quality'
```

Simplifying allows us to breath a bit, but we can still inline `let`s, add a
`baseCase`/default behavior, and use `min`/`max` to avoid explicit bounds checks:

```haskell
updateQuality :: Item -> Item
updateQuality (Item name sellIn quality)
  | name == "Aged Brie" =
       if sellIn < 1
       then Item name (sellIn - 1) $ min 50 (quality + 2)
       else baseCase (sellIn - 1) $ min 50 (quality + 1)
  | name == "Backstage passes to a TAFKAL80ETC concert" =
    let
      quality'
        | sellIn < 6 = min 50 (quality + 3)
        | sellIn < 11 = min 50 (quality + 2)
        | otherwise = min 50 (quality + 1)
    in
       if sellIn < 1
       then Item name (sellIn - 1) 0
       else baseCase (sellIn - 1) quality'
  | name == "Sulfuras, Hand of Ragnaros" =
       if sellIn < 0
       then Item name sellIn quality
       else baseCase sellIn quality
  | otherwise =
    baseCase (sellIn - 1) $ max 0 (quality - 1)
  where baseCase sellIn' quality' =
          if quality' > 0 && sellIn' < 0
          then Item name sellIn' (quality' - 1)
          else Item name sellIn' quality'
```

It's a good time to break down our big `updateQuality` function, to make a step
forward to branchless.

To do so, we can directly put it in a new datatype, `Item'`, so it'll carry the
"update" behavior alongside the data:

```haskell
data Item' = Item' { _name :: String, _sellIn :: Int, _quality :: Int, updateQuality :: Item' }

instance Show Item' where
  show (Item' name sellIn quality _) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality
```

Then, we can start to slice our function, the first issue we encounter is `baseCase`.

Currently, we have two distinct usages: as a builder (`where`),
and as a default (`otherwise`).

Let's start with the builder:

```haskell
mkBaseCase :: (Int -> Int -> Item') -> Int -> Int -> Item'
mkBaseCase f sellIn quality =
  f sellIn $
    if quality > 0 && sellIn < 0
    then quality - 1
    else quality
```

It might seem odd but, `mkBaseCase` is a higher-order function, taking the
`Item'`-builder, as such, we don't lose track of the `Item'`'s behavior.

Let's see how is goes with `baseCase`:

```haskell
baseCase :: String -> Int -> Int -> Item'
baseCase name sellIn quality =
  Item'
    { _name = name,
      _sellIn = sellIn,
      _quality = quality,
      updateQuality = mkBaseCase (baseCase name) (sellIn - 1) (max 0 (quality - 1))
    }
```

It's straightforward:

* Current `Item'` take the `name`/`sellIn`/`quality`
* Next `Item'` (`updateQuality`) is the `mkBaseCase`:
  * the builder being itself
  * `sellIn` is decreased
  * `quality` is decreased until `0`

We can derive the others mechanically:

```haskell
agedBrie :: Int -> Int -> Item'
agedBrie sellIn quality =
  Item'
    { _name = "Aged Brie",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        if sellIn < 1
        then agedBrie (sellIn - 1) $ min 50 (quality + 2)
        else mkBaseCase agedBrie (sellIn - 1) $ min 50 (quality + 1)
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
           if sellIn < 1
           then backstagePasses (sellIn - 1) 0
           else mkBaseCase backstagePasses (sellIn - 1) quality'
    }

sulfuras :: Int -> Int -> Item'
sulfuras sellIn quality =
  Item'
    { _name = "Sulfuras, Hand of Ragnaros",
      _sellIn = sellIn,
      _quality = quality,
      updateQuality =
        if sellIn < 0
        then sulfuras sellIn quality
        else mkBaseCase sulfuras sellIn quality
    }
```

Since we have a a new `Item'` data-type, we have to adapt our tests, duplicating inputs:

```haskell
spec :: Spec
spec =
  describe "Kata" $ do
    let comparedItems :: [(Item', [(Int, Item', Item)])]
        comparedItems =
          flip map initialInventory $ \(legacyItem, item) ->
            (item, zip3 [0..100] (iterate updateQuality item) (iterate updateQualityLegacy legacyItem))
    forM_ comparedItems $ \(initialItem, dailyitems) ->
      describe (show initialItem) $
        forM_ dailyitems $ \(day, actualItem, expectedItem) ->
          it ("Day " <> show day) $
            show actualItem `shouldBe` show expectedItem

initialInventory :: [(Item, Item')]
initialInventory =
  [ (Item "+5 Dexterity Vest"                          10  20, baseCase "+5 Dexterity Vest"       10  20)
  , (Item "Aged Brie"                                   2   0, agedBrie                            2   0)
  , (Item "Elixir of the Mongoose"                      5   7, baseCase "Elixir of the Mongoose"   5   7)
  , (Item "Sulfuras, Hand of Ragnaros"                  0  80, sulfuras                            0  80)
  , (Item "Sulfuras, Hand of Ragnaros"                (-1) 80, sulfuras                          (-1) 80)
  , (Item "Backstage passes to a TAFKAL80ETC concert"  15  20, backstagePasses                    15  20)
  , (Item "Backstage passes to a TAFKAL80ETC concert"  10  49, backstagePasses                    10  49)
  , (Item "Backstage passes to a TAFKAL80ETC concert"   5  49, backstagePasses                     5  49)
  -- this conjured item does not work properly yet
  , (Item "Conjured Mana Cake"                          3   6, baseCase "Conjured Mana Cake"       3   6)
  ]
```

And, that's it for today, there's still a lot to do, but this log is long-enough.
