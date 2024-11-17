+++
title = "Polysemy: NonDet"
date = 2023-02-19
draft = false
path = "2023-02/polysemy-nondet"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

As seen [in a previous log](@/blog/2023-02-12_polysemy-fixpoint.md), some effects allow type classes to be supported.

To enable `Applicative`, there is the `NonDet` effect, which works with `Error a`:

```haskell
failing :: Members '[Error Integer, Embed IO] r => Sem r Integer
failing = do
  embed $ putStrLn "failing"
  throw @Integer 1

working0 :: Members '[Error Integer, Embed IO] r => Sem r Integer
working0 = do
  embed $ putStrLn "working0"
  return 42

working1 :: Members '[Error Integer, Embed IO] r => Sem r Integer
working1 = do
  embed $ putStrLn "working1"
  return 42

actFailingFirst :: Members '[Error Integer, NonDet, Embed IO] r => Sem r Integer
actFailingFirst = failing <|> working0 <|> working1
```

Some interpreters are provided:
* `runNonDet`: produces a `Sem r (f a)` evaluating both branches of a `<|>`
* `runNonDetMaybe`: produces a `Sem r (Maybe a)` evaluating only the necessary branch of a `<|>`
* `nonDetToError`: produces a `Sem r a` relying on a `Error e` evaluating only the necessary branch of a `<|>`

Let's make some tests:

```haskell
putStrLn "# FailingFirst"
putStrLn "nonDetToError"
runM (runError @Integer $ nonDetToError @Integer 2 actFailingFirst) >>= print
putStrLn "## runNonDet"
runM (runError @Integer $ runNonDet @Maybe actFailingFirst) >>= print
putStrLn "## runNonDetMaybe"
runM (runError @Integer $ runNonDetMaybe actFailingFirst) >>= print
putStrLn "# WorkingFirst"
putStrLn "nonDetToError"
runM (runError @Integer $ nonDetToError @Integer 2 actWorkingFirst) >>= print
putStrLn "## runNonDet"
runM (runError @Integer $ runNonDet @Maybe actWorkingFirst) >>= print
putStrLn "## runNonDetMaybe"
runM (runError @Integer $ runNonDetMaybe actWorkingFirst) >>= print
```

Giving:

```
# FailingFirst
nonDetToError
failing
working0
Right 42
## runNonDet
failing
Left 1
## runNonDetMaybe
failing
Left 1
# WorkingFirst
nonDetToError
working0
Right 42
## runNonDet
working0
failing
Left 1
## runNonDetMaybe
working0
Right (Just 42)
```

Let's try to summarize:

With `failing` as first element of `<|>`:

|                | Eager | Result   |
|----------------|-------|----------|
| nonDetToError  | x     | Right 42 |
| runNonDet      |       | Left 1   |
| runNonDetMaybe |       | Left 1   |

With `working` as first element of `<|>`:

|                | Eager | Result          |
|----------------|-------|-----------------|
| nonDetToError  |       | Right 42        |
| runNonDet      | x     | Left 1          |
| runNonDetMaybe |       | Right (Just 42) |

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/NonDet.hs).
