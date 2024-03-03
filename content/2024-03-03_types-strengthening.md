+++
title = "Types strengthening"
date = 2024-03-03
draft = false
path = "2024-03/types-strengthening"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "design", "engineering", "type-driven design"]
+++

In a [previous log](@/2024-02-25_tydd-applied-librarian.md), I have introduced
[librarian](https://github.com/blackheaven/librarian), as an example of
[Type-Driven Development](@/2024-02-21_types-tests.md).

For a reminder, we had "complex" types:

```haskell
-- GADTs
data GroupingBucket :: Type -> Type where
  Daily :: GroupingBucket UTCTime
  Weekly :: GroupingBucket UTCTime
  Monthly :: GroupingBucket UTCTime

-- Existential
data Filtering
  = AllF
  | AndF Filtering Filtering
  | OrF Filtering Filtering
  | forall a. Ord a => GtF (Source a) (Source a)
  | forall a. Ord a => LtF (Source a) (Source a)
```

There are complex enough to don't be supported by the GHC `Generic` derivation,
consequently, we cannot derive `FromDhall`.

Note: I have tried to implement them myself, don't do that, even if you like pain

The simplest solution is to use an intermediate data type which would be simpler
and bounded to parsing (it is a [Data Transfer Object (DTO)](https://en.wikipedia.org/wiki/Data_transfer_object)):

```haskell
data Filtering
  = All
  | And Filtering Filtering
  | Or Filtering Filtering
  | GtTemporal SourceTemporal SourceTemporal
  | LtTemporal SourceTemporal SourceTemporal
  deriving stock (Eq, Show)

data GroupSelectionTemporal
  = AfterTemporal Int SortingOrder SourceTemporal
  | BeforeTemporal Int SortingOrder SourceTemporal
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall GroupSelectionTemporal

data SourceTemporal
  = SourceDate SourceDate
  | SourceTime TimeSpec
  deriving stock (Eq, Show, Generic)

deriving anyclass instance FromDhall SourceTemporal
```

I'm forced to specialize data types in order to keep type-safety.

There is another issue, since `Filtering` is recursive, `FromDhall` derivation
cannot be properly done, consequently, we have to use [fixpoints](https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion),
at type-level it is defined in Haskell as follows:

```haskell
newtype Fix f = Fix { unFix :: f (Fix f) }
```

The issue is that `Filtering` does not have parameter, hopefully there is a
`TemplateHaskell` way to derive it:

```haskell
TH.makeBaseFunctor ''Filtering
```

Which will generate something like that:

```haskell
data FilteringF a
  = All
  | And a a
  | Or a a
  | GtTemporal SourceTemporal SourceTemporal
  | LtTemporal SourceTemporal SourceTemporal
  deriving stock (Eq, Show)
```

Which allows us to derive `FromDhall` and to use it for our `Rule`:

```haskell
deriving stock instance Generic (FilteringF a)

deriving anyclass instance FromDhall a => FromDhall (FilteringF a)

data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    grouping :: Grouping,
    filtering :: Fix FilteringF,
    actions :: [Action]
  }
  deriving stock (Generic)

deriving anyclass instance FromDhall Rule
```

The final part consists in convert the above types (Dhall-compatible, source `S`),
to the library types (target, `T`):

```haskell
convertGrouping :: S.Grouping -> T.Grouping
convertGrouping =
  \case
    S.FileGroup -> T.FileGroup
    S.GroupTemporally source bucket selection ->
      T.Group
        { groupSource = convertSourceTemporal source,
          groupBucket = convertGroupingBucketTemporal bucket,
          groupSelection = convertGroupSelectionTemporal selection
        }

convertAction :: S.Action -> T.Action
convertAction =
  \case
    S.Move {..} -> T.Move {inputPattern = inputPattern, newName = newName}
    S.Copy {..} -> T.Copy {inputPattern = inputPattern, newName = newName}
    S.Remove {..} -> T.Remove {inputPattern = inputPattern}

convertSourceTemporal :: S.SourceTemporal -> T.Source UTCTime
convertSourceTemporal =
  \case
    S.SourceDate x -> T.SourceDate $ convertSourceDate x
    S.SourceTime x -> T.SourceTime $ convertTimeSpec x

convertGroupingBucketTemporal :: S.GroupingBucketTemporal -> T.GroupingBucket UTCTime
convertGroupingBucketTemporal =
  \case
    S.Daily -> T.Daily
    S.Weekly -> T.Weekly
    S.Monthly -> T.Monthly
```
