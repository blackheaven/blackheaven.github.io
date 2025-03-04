+++
title = "Extreme branchless: Pagination seven"
date = 2025-03-04
draft = false
path = "2025-03/extreme-branchless-pagination-seven"

[taxonomies]
categories = ["Code practice"]
tags = ["haskell", "code kata", "coding dojo", "functional programming"]
+++

Continuing my [branchless](@/blog/2024-11-05_extreme-branchess-2.md) journey.

This time: [Pagination seven](https://codingdojo.org/kata/PaginationSeven/) kata.

I'm not really a huge fan of this kata as, to me, it mostly consist in
distinguishing multiple strategies.

Let's have a minimalist start:

```haskell
spec :: Spec
spec =
  describe "Pagination seven" $ do
    forM_ [
        (2, 5, "1 (2) 3 4 5")
      ] $ \(p, t, expected) ->
        it ("Page " <> show p <> " of " <> show t) $
          paginate p t `shouldBe` expected

paginate :: Natural -> Natural -> String
paginate currentPage total = "1 (2) 3 4 5"
```

Let's force a real implementation with another tests:

```haskell
-- (6, 7, "1 2 3 4 5 (6) 7")

paginate :: Natural -> Natural -> String
paginate currentPage total = unwords $ tail $ zipWith3 (\f x y -> f x y) selector simpleNumbers checkedNumbers
  where simpleNumbers = show <$> [0 .. natInt total]
        checkedNumbers = map (\n -> "(" <> n <> ")") simpleNumbers
        selector = runNatural currentPage (const :) (flip const : repeat const)
```

It works as follows:

* `simpleNumbers` is a list of simple number (e.g. `1 2 3 4 5 6 7`)
* `checkedNumbers` is a list of selected numbers (e.g. `(1) (2) (3) (4) (5) (6) (7)`)
* `selector` is a list of functions taking it's first (e.g. `const`) or the second (e.g. `flip const`)

I zip the three lists, and here we are!

In order to go further, and add ellipses, we have to distinguish the cases
depending on the total number of pages.

We can do that by splitting the solution space in a list:

```haskell
-- (5, 9, "1 … 4 (5) 6 … 9")

paginate :: Natural -> Natural -> String
paginate currentPage total = at strategies total currentPage
  where strategies =
          error "no pagination on 0" : map (smallPaginate . fromInteger) [1 .. 7] <> map (bigPaginate . fromInteger) [8 ..]

smallPaginate :: Natural -> Natural -> String
smallPaginate total currentPage = unwords $ tail $ zipWith3 (\f x y -> f x y) selector simpleNumbers checkedNumbers
  where simpleNumbers = show <$> [0 .. natInt total]
        checkedNumbers = map (\n -> "(" <> n <> ")") simpleNumbers
        selector = runNatural currentPage (const :) (flip const : repeat const)

bigPaginate :: Natural -> Natural -> String
bigPaginate total currentPage = "1 … 4 (5) 6 … 9"
```

Let's add another test to force a real implementation:

```haskell
-- (42, 100, "1 … 41 (42) 43 … 100")

paginate :: Natural -> Natural -> String
paginate currentPage total = at strategies total currentPage
  where strategies =
          error "no pagination on 0" : map (smallPaginate . fromInteger) [1 .. 7] <> map (bigPaginate . fromInteger) [8 ..]

bigPaginate :: Natural -> Natural -> String
bigPaginate total currentPage = "1 … " <> at centers currentPage <> " … " <> show total
  where centers = map (\n -> show n <> " (" <> show (n + 1) <> ") " <> show (n + 2)) [-1 :: Int .. ]
```

Then, when have to handle when the current page is close to the
start, again, we can split the solution space with a list, this time indexed on
the current page:

```haskell
-- (4, 9, "1 2 3 (4) 5 … 9")

paginate :: Natural -> Natural -> String
paginate currentPage total = at strategies total currentPage
  where strategies =
          error "no pagination on 0" : map (smallPaginate . fromInteger) [1 .. 7] <> map (bigPaginate . fromInteger) [8 ..]

bigPaginate :: Natural -> Natural -> String
bigPaginate total currentPage = at strategies currentPage
  where strategies =
          error "no pagination on 0"
            : map (\n -> smallPaginate 5 (fromInteger n) <> " … " <> show total) [1 .. 4] <> repeat onCenter
        onCenter = "1 … " <> at centers currentPage <> " … " <> show total
        centers = map (\n -> show n <> " (" <> show (n + 1) <> ") " <> show (n + 2)) [-1 :: Int .. ]
```

Lastly, we have to handle when current is close to the end.

To simplify the implementation, we'll generalize and reuse `smallPaginate`:

```haskell
-- (6, 9, "1 … 5 (6) 7 8 9")

paginate :: Natural -> Natural -> String
paginate currentPage total = at strategies total currentPage
  where strategies =
          error "no pagination on 0"
            : map (smallPaginate . fromInteger) [1 .. 7]
            <> map (bigPaginate . fromInteger) [8 ..]

smallPaginate :: Natural -> Natural -> String
smallPaginate = smallPaginateFrom 0

smallPaginateFrom :: Natural -> Natural -> Natural -> String
smallPaginateFrom from total currentPage =
    unwords $ runNatural from tail $ tail $ zipWith3 (\f x y -> f x y) selector simpleNumbers checkedNumbers
  where simpleNumbers = show <$> [0 .. natInt total]
        checkedNumbers = map (\n -> "(" <> n <> ")") simpleNumbers
        selector = runNatural currentPage (const :) (flip const : repeat const)

bigPaginate :: Natural -> Natural -> String
bigPaginate total currentPage = at strategies currentPage
  where strategies =
          error "no pagination on 0"
            : map (\n -> smallPaginate 5 (fromInteger n) <> " … " <> show total) [1 .. 4]
            <> replicate (natInt $ total - 8) onCenter
            <> map (\n -> "1 … " <> smallPaginateFrom (total - 5) total (total - 4 + fromInteger n)) [1 .. 4]
        onCenter = "1 … " <> at centers currentPage <> " … " <> show total
        centers = map (\n -> show n <> " (" <> show (n + 1) <> ") " <> show (n + 2)) [-1 :: Int .. ]
```

Here we are!
