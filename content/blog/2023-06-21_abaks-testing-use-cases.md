+++
title = "Abaks: Testing Use Cases"
date = 2023-06-21
draft = false
path = "2023-06/abaks-testing-use-cases"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

The [Use Cases](@/blog/2023-06-18-abaks-use-cases.md) is a simple application-dependant wiring layer,
however, I tend to have one or two tests to ensure that `Interpreters` are well coordinating (usually in the nominal case).

But unlike [Core domain/Entities](@/blog/2023-06-14_abaks-testing-core.md), it'll require a bit more work.

Let's review the test case:

```haskell
describe "UseCases" $
  around withFixture $ do
    it "All chained commands should work" $ \fixture ->
      runUsecase
        fixture
        ( runError @Text $ do
            periodId <- createPeriod periodName periodStart periodEnd (Amount 0) >>= fromEither
            addEntry periodId anEntry >>= fromEither
            changeAmountEntry periodId anEntry.entryId (Amount 1500) >>= fromEither
            validateEntry periodId anEntry.entryId >>= fromEither
            commentEntry periodId anEntry.entryId "The Hateful 8: too cool" >>= fromEither
            markInClonflictEntry periodId anEntry.entryId "way too expensive" >>= fromEither
            deleteEntry periodId anEntry.entryId "wrong period" >>= fromEither
        )
        `shouldReturn` Right ()
```

It's roughly a copy-paste of one of a nominal test case of the core domain.
We relied on `fromEither` to shortcut de process (it will generate an `Error Text` if a `Left Text` occurs).

Note that it is also a way to describe how use cases are supposed to be chained.

One thing to notice is the `runUsecase fixture` used to run Use Cases defined as:

```haskell
newtype Fixture = Fixture
  { runUsecase :: forall a. Sem '[EventSourceEffect AbaksEvent, Final IO] a -> IO a
  }
```

`hspec` not being able to run `Sem r a` directly.

We also need to inject (the `Args` of the `it`, `\fixtures ->`), this is done through `around withFixture`:

```haskell
withFixture :: ActionWith Fixture -> IO ()
withFixture action = action $ Fixture $ runFinal . runMemoryUnsafe
```

Alternatively, we could also have directly a final function to do it, which would have been simpler.

It would have been, even though, in real world you may have additional context (a database, etc.) you want to inject, you may also want to change interpretation.
