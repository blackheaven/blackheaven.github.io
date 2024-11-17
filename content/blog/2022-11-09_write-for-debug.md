+++
title = "Write for debug"
date = 2022-11-09
draft = false
path = "2022-11/write-for-debug"

[taxonomies]
categories = ["Design"]
tags = ["gdcr", "code kata", "coding dojo", "code retreat", "design"]
+++

In my last session of the [GDCR](@/blog/2022-11-06_gdcr-summary.md), during the Red phase I ended up with this test:

```haskell
spec :: Spec
spec =
  describe "SpaceX" $ do
    describe "computeLaunchCost" $ do
      it "Reused rocket should be launch only" $
        computeLaunchCost ReusedRocket `shouldBe` launchCost
      it "First launched rocket should be launch and build" $
        computeLaunchCost FirstLaunchedRocket `shouldBe` (launchCost <> buildCost)
```

And thie result:

```
  test/Spec.hs:17:47:
  1) SpaceX.computeLaunchCost First launched rocket should be launch and build
       expected: Cost {getCost = 2873037}
        but got: Cost {getCost = 336552}
```

At this time, I lack adding the build cost, which is normal.

But what if I came back several days/weeks later on it?

I would have to:

1. Figure out how the computation is done
2. Compute the diffrence
3. Find out that this number is the build cost
4. Check that the specs are confirming it

However, if I change my design so:

```haskell
newtype Cost = Cost {getCost :: Int}
  deriving stock (Eq, Ord, Show)
```

become

 ```haskell
data Cost = Cost
  { builds :: Int,
    launches :: Int
  }
  deriving stock (Eq, Ord, Show)
```

then failure is obvious:

```
  1) SpaceX.computeLaunchCost First launched rocket should be launch and build
       expected: Cost {builds = 1, launches = 1}
        but got: Cost {builds = 0, launches = 1}
```
