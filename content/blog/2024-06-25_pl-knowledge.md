+++
title = "In-depth programming language is not the point"
date = 2024-06-25
draft = false
path = "2024-06/pl-knowledge"

[taxonomies]
categories = ["Haskell"]
tags = ["engineering", "haskell", "algorithms"]
+++

When I was younger, I was focused on competitive programming, not a lot, but way
more than in the last decade, at some point, I have attended to [Prologin](https://prologin.org/)
which is limited people being at most 20 years old.

I have managed to in the top 10 of the qualification phase, and top 5 of the
regional events, while I was only using PHP (yes, I was beating C/C++ and Java code).

I cannot deny that my knowledge on PHP at the time helped me to earn some points,
but most of my "success" was due to the selection of the right algorithms.

Since few months, I train lightly on [leetcode](https://leetcode.com), to,
amongst [other things](@/blog/2024-03-24_streak.md), train to Rust, and I have
crossed a classical [dynamic programming](https://en.wikipedia.org/wiki/Dynamic_programming)
(I have finally nailed after 20 years) problem: [Coin change](https://leetcode.com/problems/coin-change/description/).

The problem is simple: you have coin types (you can use as many of them as you
want) and a target amount, you must give the minimal number of coins you need
to reach that target.

Note: in the benchmarks below, I have picked:

```haskell
coins = [1, 2, 3, 5, 7, 8, 11, 17]
target = 75
```

The first strategy is simple: pick the `target`, for all `coins`, try to find the
minimal `number of coins` it takes to have `target - coin`, add `1`, do it
recursively.

Simple, recursive, "functional":

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount = fromMaybe (-1) $ go (reverse $ sort baseCoins) targetAmount
  where
    go :: [Int] -> Int -> Maybe Int
    go _ 0 = Just 0
    go [] _ = Nothing
    go (c : cs) n
      | c > n = go cs n
      | otherwise =
          case ((+ 1) <$> go (c : cs) (n - c), go cs n) of --
            (Just x, Just y) -> Just $ min x y
            (x, y) -> x <|> y
```

But incredibly slow:

```
time                 755.7 ms   (737.8 ms .. 775.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 755.5 ms   (752.3 ms .. 758.3 ms)
std dev              3.380 ms   (1.665 ms .. 4.604 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Not so good, my knowledge of Haskell gives me two smells: explicit recursion
is bad and accumulating using returned result prevents [Tail call optimization](https://wiki.haskell.org/Tail_recursion).

Let's tackle the first one with [`fix`]('https://hackage.haskell.org/package/base/docs/Data-Function.html#v:fix' ):

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount = fromMaybe (-1) $ fix go (reverse $ sort baseCoins) targetAmount
  where
    go :: ([Int] -> Int -> Maybe Int) -> [Int] -> Int -> Maybe Int
    go _ _ 0 = Just 0
    go _ [] _ = Nothing
    go r (c : cs) n
      | c > n = go r cs n
      | otherwise =
          case ((+ 1) <$> r (c : cs) (n - c), r cs n) of --
            (Just x, Just y) -> Just $ min x y
            (x, y) -> x <|> y
```

```
time                 754.7 ms   (740.9 ms .. 771.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 760.9 ms   (757.8 ms .. 765.3 ms)
std dev              4.124 ms   (174.5 μs .. 5.113 ms)
variance introduced by outliers: 19% (moderately inflated)
```

No change.

Let's be optimistic, can we enable TCO:

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount = fromMaybe (-1) $ fix go 0 (reverse $ sort baseCoins) targetAmount
  where
    go :: (Int -> [Int] -> Int -> Maybe Int) -> Int -> [Int] -> Int -> Maybe Int
    go _ nb _ 0 = Just nb
    go _ _ [] _ = Nothing
    go r nb (c : cs) n
      | c > n = go r nb cs n
      | otherwise =
          case (r (nb + 1) (c : cs) (n - c), r nb cs n) of
            (Just x, Just y) -> Just $ min x y
            (x, y) -> x <|> y
```

```
time                 565.3 ms   (534.5 ms .. 607.7 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 557.9 ms   (552.2 ms .. 562.8 ms)
std dev              6.064 ms   (2.872 ms .. 8.158 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Roughly 25% improvement, not great.

The main issue is that the same targets are checked over and over again, which is
a waste.

Note: most of the time, optimizing a piece of code is only a matter of coming up
with a better *problem* design, focusing on the data encountered, reducing wasted
CPU cycles.

Instead, for each amount up-to the target we compute the minimal `number of coins`:

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount = fromMaybe (-1) $ V.unsafeIndex solutions targetAmount
  where
    solutions = foldl' go start [1 .. targetAmount]
    start =
      V.fromList (Just 0 : replicate targetAmount Nothing)
        V.// zip (takeWhile (<= targetAmount) baseCoins) (repeat $ Just 1)
    go acc n = V.unsafeUpd acc [(n, solve acc n)]
    solve ps n =
      (ps V.! n)
        <|> safeMinimum
          ( mapMaybe (\x -> on (liftA2 (+)) (ps V.!) x (n - x)) $
              dropWhile (>= n) reversedBaseCoins
          )

    safeMinimum xs = if null xs then Nothing else Just $ minimum xs
    reversedBaseCoins = reverse baseCoins
```

```
time                 406.8 μs   (404.5 μs .. 409.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 406.4 μs   (404.8 μs .. 408.8 μs)
std dev              6.134 μs   (4.169 μs .. 10.07 μs)
```

138962% improvement, not bad.

Can we do better?

Fortunately, [`vector`](https://hackage.haskell.org/package/vector) (used to
have efficient random access, `O(1)`) has a [mutable API](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Mutable.html).

Let's rewrite it in that style:

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount =
  runST $ do
    ss <- VM.replicate (targetAmount + 1) Nothing
    VM.write ss 0 $ Just 0
    forM_ (takeWhile (<= targetAmount) baseCoins) $ \n ->
      VM.write ss n $ Just 1
    forM_ [1 .. targetAmount] $ \n -> do
      let go previousM =
            case previousM of
              Nothing ->
                safeMinimum
                  . catMaybes
                  <$> mapM
                    (\x -> on (liftA2 (liftA2 (+))) (VM.unsafeRead ss) x (n - x))
                    (dropWhile (>= n) reversedBaseCoins)
              Just p -> return $ Just p
      VM.unsafeModifyM ss go n
    fromMaybe (-1) <$> VM.unsafeRead ss targetAmount
  where
    safeMinimum xs = if null xs then Nothing else Just $ minimum xs
    reversedBaseCoins = reverse baseCoins
```

That was painful, did it payed-off?

```
time                 439.4 μs   (436.9 μs .. 442.1 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 441.3 μs   (439.1 μs .. 443.9 μs)
std dev              8.096 μs   (5.966 μs .. 11.40 μs)
```

8% slower.

> There are two kinds of pain.
> The sort of pain that makes you strong, or useless pain.
> The sort of pain that's only suffering.
> I have no patience for useless things.
> 
> - Francis Underwood

My guess is, either I don't know how to properly use the mutable API, or GHC
(Haskell main compiler) is great at optimizing regular code.

Sure, you need a minimal knowledge of the programming language [(syntax,
build tools, general rules of thumb, customs/usages)](@/blog/2023-11-12_language-tourist.md),
but, aside of that, the approach (which depends more on the general
knowledge/culture) is more important (for performance and maintainability).
Note to mention that performance is rarely a concern.

Note: while writing this article, I have found an older one on [School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/coinChange):

```haskell
coinAmount :: [Int] -> Int -> Int
coinAmount baseCoins targetAmount = maybe (-1) fst $ get ltCoin targetAmount
  where
    arr = A.array ((0, 0), (ltCoin, targetAmount)) [((i, c), takeC i c) | i <- [0 .. ltCoin], c <- [0 .. targetAmount]]
    get i c
      | c < 0 || i < 0 = Nothing
      | c == 0 = Just (0, [])
      | otherwise = arr A.! (i, c)
    ltCoin = length reversedCoins - 1
    reversedCoins = reverse baseCoins
    takeC cNr cts
      | coin > cts = get (cNr - 1) cts
      | otherwise = case (get cNr (cts - coin), get (cNr - 1) cts) of
          (Just (n, t), Just (n', t')) -> Just $ if n + 1 <= n' then (n + 1, coin : t) else (n', t')
          (Nothing, Just (n', t')) -> Just (n', t')
          (Just (n, t), Nothing) -> Just (n + 1, coin : t)
          (Nothing, Nothing) -> Nothing
      where
        coin = reversedCoins !! cNr
```

```
time                 1.181 ms   (1.174 ms .. 1.188 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.180 ms   (1.176 ms .. 1.186 ms)
std dev              16.28 μs   (11.76 μs .. 22.92 μs)
```

290% slower
