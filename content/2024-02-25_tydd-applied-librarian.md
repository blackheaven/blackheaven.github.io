+++
title = "Type-driven development applied: librarian"
date = 2024-02-25
draft = false
path = "2024-02/tydd-applied-librarian"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "design", "engineering", "type-driven design", "test-driven development"]
+++

In my [previous log](@/2024-02-21_types-tests.md) I have introduced _Type-Driven Development_,
it was an overview of how I practice.
I feel the need to show concretely how it works.

A long time ago, I worked in a company which had customers who were pushing
data through a [FTP server](https://en.wikipedia.org/wiki/FTP_server).
The problem was that each customer had their own naming convention, to simplify
the ingestion in our [ETL](https://en.wikipedia.org/wiki/Extract,_transform,_load),
I have created [librarian](https://github.com/blackheaven/librarian) few
times ago to mimic a regex-based ruleset to rename/normalize filenames.

It works as follows:

Create a [dhall file](https://dhall-lang.org/) expressing your renaming patterns:

```dhall
[
  { name = "All footages"
  , match = "CCTV/**/*.*m*"
  , movers = [
        { inputPattern = "CCTV/(.*)/(\\d+)\\.mp4"
        , newName = "sorted/\\2/\\1.mp4"
        }
    ]
  }
]
```

Run _librarian_ and review moves:

```
$ librarian -r examples/cctv.dhall
[All footages] './CCTV/garage/220520.mp4' -> './sorted/220520/garage.mp4'
[All footages] './CCTV/garage/220521.mp4' -> './sorted/220521/garage.mp4'
[All footages] './CCTV/garage/220522.mp4' -> './sorted/220522/garage.mp4'
[All footages] './CCTV/garage/220523.mp4' -> './sorted/220523/garage.mp4'
[All footages] './CCTV/garden/220520.mp4' -> './sorted/220520/garden.mp4'
[All footages] './CCTV/garden/220521.mp4' -> './sorted/220521/garden.mp4'
[All footages] './CCTV/garden/220522.mp4' -> './sorted/220522/garden.mp4'
[All footages] './CCTV/garden/220523.mp4' -> './sorted/220523/garden.mp4'
Move? (y/n)
```

Currently, it is architected around three functions:

```haskell
fetchRulesOn :: FilePath -> [Rule] -> IO CollectedFiles
planMoves :: CollectedFiles -> [Move]
runPlan :: [Move] -> IO RunResult
```

It aims to be used as follows:

- `fetchRulesOn`: apply rules on each file and associate files to a rule to apply
- `planMoves`: convert file/rule to actions (limited to move/rename)
- `runPlan`: apply moves on the file system

In my current position we have another concern: backups garbage collection.

Briefly: we are operating many microservices with their own data-stores, multiple
times a day we run snapshots to have backups, they are done on each machine
and copied on an AWS S3 bucket (replicated on multiple cold storages), the
thing is, we want to keep some depth in local backups, but in a smart way:

* All snapshots of the last month
* One snapshot a day on the last 1-3 months
* One snapshot a week on the last 3-6 months
* One snapshot per month after 6 months

We have to change our existing, so we would be able to:

* Have other actions than move (copy, remove)
* Have multiple rules applied to a single file (for example: copy, copy, and move)
* Group and select files

You might have noticed the order is reverse of our "business" value.

I have intentionally ordered them, so I have small steps.

Let start with `Move` which will `ResolvedAction`, and `Mover` to `Action`,
then we can add some tests:

```haskell
describe "fetchRulesOn" $ do
  describe "move" $ do
    it "Text files only should match only all rule" $
      withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." moveRules)
        `shouldReturn` Map.fromList [("./in/sub/0.txt", moveRule1Any), ("./in/1.txt", moveRule1Any)]
  describe "copy" $ do
    it "Text files only should match only all rule" $
      withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." copyRules)
        `shouldReturn` Map.fromList [("./in/sub/0.txt", copyRule1Any), ("./in/1.txt", copyRule1Any)]
describe "planActions" $ do
  describe "move" $ do
    it "Images should be moved, texts should have their extension changed" $
      planActions (Map.fromList [("./in/sub/0.jpg", moveRule0Jpg), ("./in/1.txt", moveRule1Any)])
        `shouldBe` [ ResolvedMove "./in/1.txt" "./in/1.TXT" moveRule1Any,
                     ResolvedMove "./in/sub/0.jpg" "out/pics/0.jpg" moveRule0Jpg
                   ]
  describe "copy" $ do
    it "Images should be copyd, texts should have their extension changed" $
      planActions (Map.fromList [("./in/sub/0.jpg", copyRule0Jpg), ("./in/1.txt", copyRule1Any)])
        `shouldBe` [ ResolvedCopy "./in/1.txt" "./in/1.TXT" copyRule1Any,
                     ResolvedCopy "./in/sub/0.jpg" "out/pics/0.jpg" copyRule0Jpg
                   ]
describe "runPlan" $ do
  describe "move" $ do
    let moveAll = fetchRulesOn "." [moveAllTxtRule] >>= runPlan . planActions
    it "Overriding paths should block the second move" $
      withFiles ["in/0.txt", "in/sub/0.txt"] moveAll
        `shouldReturn` [ (FsMove "./in/0.txt" "out/0.txt", Done),
                         (FsMove "./in/sub/0.txt" "out/0.txt", Existing)
                       ]
    it "Overriding paths should keep the second file" $
      withFiles ["in/0.txt", "in/sub/0.txt"] (moveAll >> listFiles)
        `shouldReturn` ["./in/sub/0.txt", "./out/0.txt"]
  describe "copy" $ do
    let copyAll = fetchRulesOn "." [copyAllTxtRule] >>= runPlan . planActions
    it "Overriding paths should block the second copy" $
      withFiles ["in/0.txt", "in/sub/0.txt"] copyAll
        `shouldReturn` [ (FsCopy "./in/0.txt" "out/0.txt", Done),
                         (FsCopy "./in/sub/0.txt" "out/0.txt", Existing)
                       ]
    it "Overriding paths should keep the second file" $
      withFiles ["in/0.txt", "in/sub/0.txt"] (copyAll >> listFiles)
        `shouldReturn` ["./in/0.txt", "./in/sub/0.txt", "./out/0.txt"]
```

Tests are mostly copy-pasted (their behaviors are really close), but it forces
us to add `*Copy` constructors:

```haskell
data Action
  = Move {inputPattern :: String, newName :: String}
  | Copy {inputPattern :: String, newName :: String}
  deriving stock (Eq, Show, Generic)

data ResolvedAction
  = ResolvedMove {original :: FilePath, new :: FilePath, rule :: Rule}
  | ResolvedCopy {original :: FilePath, new :: FilePath, rule :: Rule}
  deriving stock (Eq, Show, Generic)

data FsAction
  = FsMove {from :: FilePath, to :: FilePath}
  | FsCopy {from :: FilePath, to :: FilePath}
  deriving stock (Eq, Show, Generic)
```

Doing so we move from a product type to a sum type, which forces us to add `case`s
everywhere (that's what we want):

```haskell
displayPlan :: [ResolvedAction] -> IO ()
displayPlan =
  mapM_ $ \case
    ResolvedMove {..} ->
      putStrLn $ "Move [" <> getRuleName (name rule) <> "] '" <> original <> "' -> '" <> new <> "'"
    ResolvedCopy {..} ->
      putStrLn $ "Copy [" <> getRuleName (name rule) <> "] '" <> original <> "' -> '" <> new <> "'"
```

Secondly, we want to support multiple `Action`s per files, this time we start
with types, especially `CollectedFiles` which associate files to rule:

```haskell
-- Before
type CollectedFiles = Map.Map FilePath Rule

-- After
type CollectedFiles = Map.Map FilePath (Seq Rule)
```

Note: `Seq` is a `List`-like container

Changes are minimal (a flattening here and there), internal tests are impacted
a bit:

```haskell
describe "fetchRulesOn" $ do
  describe "move" $ do
    it "Text files only should match only all rule" $
      withFiles ["in/sub/0.txt", "in/1.txt"] (fetchRulesOn "." moveRules)
        `shouldReturn` Map.fromList [("./in/sub/0.txt", [moveRule1Any]), ("./in/1.txt", [moveRule1Any])]
    it "Text/images files should match twice" $
      withFiles ["in/sub/0.jpg", "in/1.txt"] (fetchRulesOn "." moveRules)
        `shouldReturn` Map.fromList [("./in/sub/0.jpg", [moveRule0Jpg, moveRule1Any]), ("./in/1.txt", [moveRule1Any])]
```

Notice how the last test case gives two matching rules.

Then we can add a comprehensive test:

```haskell
describe "runPlan" $ do
  describe "mixed" $ do
    let mixed = fetchRulesOn "." [copyAllTxtRule, removeAllTxtRule] >>= runPlan . planActions
    it "Should copy and delete" $
      withFiles ["in/0.txt"] (mixed >> listFiles)
        `shouldReturn` ["./out/0.txt"]
```

Finally, we have the most challenging part: group and select files.

First, let's sketch an expression to model our use case:

```haskell
Rule
  { name = "Purge old text files",
    match = "**/*.txt",
    grouping =
      Group
        { groupCriteria = ByDate (SourceDate CreationTime) Daily,
          groupSelection = After 0 SortingAsc (SourceDate CreationTime)
        },
    filtering =
      GtF (SourceDate CreationTime) (SourceTime $ DaysAgo 31)
        `AndF` LtF (SourceDate CreationTime) (SourceTime $ DaysAgo 92),
    actions = [Remove "^.*/([^\\/]+)$"]
  }
```

We can start modifying our `Rule`:

```haskell
data Rule = Rule
  { name :: RuleName,
    match :: Matcher,
    grouping :: Grouping,
    filtering :: Filtering,
    actions :: [Action]
  }
```

Then, there is a bunch of setup to do:

```haskell
data Grouping
  = FileGroup
  | Group {groupCriteria :: GroupingCriteria, groupSelection :: GroupSelection}

data Source :: Type -> Type where
  SourceDate :: SourceDate -> Source Date
  SourceTime :: TimeSpec -> Source Date

data SourceDate
  = CreationTime
  | AccessTime

data TimeSpec
  = HoursAgo Int
  | DaysAgo Int
  | AbsoluteTime Date

data SortingOrder
  = SortingAsc
  | SortingDesc

data GroupSelection
  = AllGS
  | forall a. After Int SortingOrder (Source a)
  | forall a. Before Int SortingOrder (Source a)

data GroupingCriteria
  = forall a. ByDate (Source a) (GroupingBucket a)

data GroupingBucket :: Type -> Type where
  Daily :: GroupingBucket Date
  Weekly :: GroupingBucket Date
  Monthly :: GroupingBucket Date

data Filtering
  = AllF
  | AndF Filtering Filtering
  | OrF Filtering Filtering
  | forall a. GtF (Source a) (Source a)
  | forall a. LtF (Source a) (Source a)
```

Finally, we can change our implementation, but this is mostly implementation
details (and this log is long enough).
