+++
title = "Types for building and types for running"
date = 2024-03-06
draft = false
path = "2024-03/types-building-running"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "design", "engineering", "type-driven design"]
+++

Since I have introduced [Type-Driven Development](@/blog/2024-02-21_types-tests.md),
I have applied it to [business logic](@/blog/2024-02-25_tydd-applied-librarian.md),
to [tests](@/blog/2024-02-28_edsls-tests.md), to [interfacing](@/blog/2024-03-03_types-strengthening.md),
now I want to distinguish types by usage stages.

A long time ago, I worked on a linter which was working on [XML documents](https://en.wikipedia.org/wiki/XML).

An XML document is a tree of named nodes:

```haskell
newtype Tree = TreeNode [(String, Tree)]
  deriving stock (Eq, Show)
```

I had a Java program with were defining roughly 100 rules associated to node paths:

```haskell
data LintSpec = LintSpec
  { path :: String,
    lintSpec :: NodeLint
  }
  deriving stock (Eq, Show)

runLint :: NodeLint -> Tree -> [LintResult]
```

The ruleset was collected in the codebase and applied one by one to hundreds
of files:

```haskell
lintNaive :: [LintSpec] -> Tree -> [LintResult]
lintNaive specs tree =
  [ result
    | spec <- specs,
      targetNode <- findNodes (splitPath spec.path) tree,
      result <- runLint spec.lintSpec targetNode
  ]
  where
    findNodes :: [String] -> Tree -> [Tree]
    findNodes ps' tree' =
      case (ps', tree') of
        ([], _) -> [tree']
        (p : ps, TreeNode ts) -> concatMap (findNodes ps . snd) $ filter ((== p) . fst) ts
    splitPath =
      unfoldr $
        \remaining ->
          case break (== '/') remaining of
            ("", _) -> Nothing
            (p, ps) -> Just (p, ps)
```

The result was really, really bad, it was taking 45 minutes.

The issue was caused by a lack of awareness of the search space, so, each
lint path was taken independently, and the tree was visited entirely each time
which gives a time complexity of `O(number-of-rules * number-of-matching-node)`.

My hint was that most of the rules where operating on the same nodes, so we could
traverse nodes only once.

I have picker [prefix tree](https://en.wikipedia.org/wiki/Trie) since we can
view top-down exploration as a tree.

The idea was to, first define the built data types:

```haskell
data LintRule = LintRule {subRules :: LintRules, lints :: [NodeLint]}
  deriving stock (Eq, Show)

newtype LintRules = LR {lintRules :: Map.Map String LintRule}
  deriving newtype (Eq, Show, Semigroup, Monoid)
```

Then to have a trivial way to build them from a list of lints:

```haskell
buildRules :: [LintSpec] -> LintRules
buildRules = foldl' addNode mempty . map splitByPath
  where
    splitByPath spec = (splitPath spec.path, spec.lintSpec)
    splitPath =
      unfoldr $
        \remaining ->
          case break (== '/') remaining of
            ("", _) -> Nothing
            (p, ps) -> Just (p, ps)
    addNode :: LintRules -> ([String], NodeLint) -> LintRules
    addNode (LR rules) (path, lint) =
      LR $
        case path of
          [p] ->
            Map.alter
              (Just . maybe (LintRule mempty [lint]) (\rule -> rule {lints = lint : rule.lints}))
              p
              rules
          (p : ps) ->
            let addOn sn = addNode sn (ps, lint)
             in Map.alter
                  (Just . maybe (LintRule (addOn (LR mempty)) mempty) (\rule -> rule {subRules = addOn rule.subRules}))
                  p
                  rules
```

And finally to run it tree by tree:

```haskell
lintFromRules :: LintRules -> Tree -> [LintResult]
lintFromRules rules tree@(TreeNode nodes) = concatMap runSubRules nodes
  where
    runSubRules :: (String, Tree) -> [LintResult]
    runSubRules (nodeName, node) =
      let subRule = Map.lookup nodeName rules.lintRules
       in concatMap (flip runLint tree) (maybe mempty (.lints) subRule)
            ++ lintFromRules (maybe mempty (.subRules) subRule) node
```

This code is interesting as it mostly consist of zipping the XML Tree and the
prefix tree, ensuring only an only one traversal.

Moreover, building `LintRules` is only done once, which reduce the work to the
minimum.

Which lead to a running time of 30 seconds, on one thread.

* In order to speed up, we have to do less.
* In order to do less we have to have a clear idea of the concepts.
* In order to have a clear idea of the concepts, we have to decouple them.

Sometime decoupling is just having two functions, some other time we need two
types, task oriented.
