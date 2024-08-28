+++
title = "eDSLs for tests"
date = 2024-02-28
draft = false
path = "2024-02/edsls-tests"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "design", "engineering", "type-driven design", "test-driven development"]
+++

In the [previous log](@/2024-02-25_tydd-applied-librarian.md), I have introduced
[librarian](https://github.com/blackheaven/librarian), as an example of
[Type-Driven Development](@/2024-02-21_types-tests.md).

Last log ended up with time-related feature.

For reference, we previously had tests like this:

```haskell
describe "remove" $ do
  let removeAll = fetchRulesOn "." [removeAllTxtRule] >>= runPlan . planActions
  it "Should keep the non-matching file" $
    withFiles ["in/0.txt", "in/sub/0.txt", "in/0.jpg"] removeAll
      `shouldReturn` [ (FsRemove "./in/0.txt", Done),
                       (FsRemove "./in/sub/0.txt", Done)
                     ]
```

Now, we have to add time-related concepts, I have two options:

* replace all the occurrences with a bunch of records or functions (forcing us to change all tests)
* build an [Embedded domain specific language (eDSL)](https://wiki.haskell.org/Embedded_domain_specific_language) which is a way to build a small language dedicated to a problem

The first step is to define a datatype with all we need:

```haskell
data FileSpec = FileSpec
  { path :: FilePath,
    accessTime :: Maybe TimeSpec,
    modificationTime :: Maybe TimeSpec
  }
  deriving stock (Eq, Show)
```

Which can be used as follows:

```haskell
withFiles :: [FileSpec] -> IO a -> IO a
withFiles files act =
  withSystemTempDirectory "librarian-tests" $ \d ->
    withCurrentDirectory d $ do
      mapM_ touch files
      act

touch :: FileSpec -> IO ()
touch target = do
  createDirectoryIfMissing True $ fst $ splitFileName target.path
  writeFile target.path "-"
  let computeTime =
        \case
          HoursAgo d -> addUTCTime (secondsToNominalDiffTime $ (-1) * fromInteger d * 60 * 60) <$> getCurrentTime
          DaysAgo d -> addUTCTime ((-1) * fromInteger d * nominalDay) <$> getCurrentTime
          AbsoluteTime x -> return x
  forM_ target.accessTime $ setAccessTime target.path <=< computeTime
  forM_ target.modificationTime $ setModificationTime target.path <=< computeTime
```

Then we can simply rely on GHC [OverloadedString](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html) extension:

```haskell
instance IsString FileSpec where
  fromString p =
    FileSpec {path = p, accessTime = Nothing, modificationTime = Nothing}
```

At this point, all our tests are compiled and executed as before.

Finally, we can write our tests:

```haskell
describe "time-based" $ do
  let timeBased = fetchRulesOn "." [removeAllMidOldTtxt] >>= runPlan . planActions
  it "Should keep older and younger file" $
    withFiles
      [ "in/1.txt" {modificationTime = Just $ DaysAgo 1},
        "in/7.txt" {modificationTime = Just $ DaysAgo 7},
        "in/32.txt" {modificationTime = Just $ DaysAgo 32},
        "in/61.txt" {modificationTime = Just $ DaysAgo 61},
        "in/81.txt" {modificationTime = Just $ DaysAgo 81},
        "in/101.txt" {modificationTime = Just $ DaysAgo 101}
      ]
      (timeBased >> listFiles)
      `shouldReturn` ["./in/1.txt", "./in/101.txt", "./in/32.txt", "./in/7.txt"]
```

I often hear that functional programming does not scale, and that it only work
in the small (and we need object-oriented programming in the large).

The thing is, functional programming does not suffer from stacking components
(since types ensure strict boundaries).

eDSLs are the magic ingredient, each component can be seen as the start and the
end of systems, abstracting used abstractions while exposing a coherent, not
leaking (hopefully) abstractions.
