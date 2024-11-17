+++
title = "Abaks: testing core domain"
date = 2023-06-14
draft = false
path = "2023-06/abaks-testing-core"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

[Previous log](@/blog/2023-06-11_abaks-commands.md) saw the introduction of `Command`s.

One of the reason I favor `Clean Architecture` specifically over `Hexagonal Architecture` is to have a pure core domain, easily testable (without mocks or specific interpreters).

I also chose to build tested `Command`'s `Event`'s relying on previous `Command`s to avoid testing impossible states.

Note that the lack of legacy is also a strong motivation.

We can start with a simple helper which apply `Command`s one after the other, keeping results and accumulating `Event`s:

```haskell
applyCommands ::
  [CommandHandler AbaksEvent ExplainedError] ->
  [Either ExplainedError (Events AbaksEvent)]
applyCommands = snd . foldl' go (mempty, mempty)
  where
    go (events, previousResults) handler =
      let result = applyCommand handler events
       in (either (const events) (events <>) result, previousResults <> [result])
```

Then we can start to write some tests:

```haskell
spec :: Spec
spec = do
  describe "Entities" $ do
    it "startPeriod should work" $
      applyCommands [startPeriod anyPeriodId periodName periodStart periodEnd (Amount 0)]
        `shouldSatisfy` liftEq ($) [isRight]
    it "deleteEntry should fail" $
      applyCommands [deleteEntry (EntryId 42) "do not exist"]
        `shouldSatisfy` liftEq ($) [isLeft]
```
