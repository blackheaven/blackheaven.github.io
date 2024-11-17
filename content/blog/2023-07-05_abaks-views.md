+++
title = "Abaks: Views"
date = 2023-07-07
draft = false
path = "2023-07/abaks-views"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

The last part, in order to make our API is to be able to query it, that's why we have to introduce _Views_.

Since our project is rather simple, we have made [opinionated design choices](@/blog/2023-06-11_abaks-commands.md) in our [CQRS](https://martinfowler.com/bliki/CQRS.html) implementation, we need a _View Model_, but instead of having _Materialized Views_ or _Projections_, we'll based our implementation directly on the _Events_.

Noticeably, we want to be able to get the _Period_:

```haskell
data Period = Period
  { periodId :: PeriodId,
    name :: Text,
    from :: Day,
    to :: Day,
    initialBalance :: Amount,
    balance :: Amount,
    entries :: Map.Map EntryId Entry
  }
```

We could then define a _View_:

```haskell
type View a r = Events a -> r

viewPeriod :: View AbaksEvent (Maybe Period)
viewPeriod =
  withStartedEvent $ \started ->
    foldl' go $
      Period
        { periodId = started.periodId,
          name = started.name,
          from = started.from,
          to = started.to,
          balance = started.initialBalance,
          initialBalance = started.initialBalance,
          entries = mempty
        }
  where
    go acc =
      \case
        Started _ -> acc
        EntryAdded e ->
          adjustAmount (Amount 0) e.entry.amount $
            withEntries acc $
              Map.insert e.entry.entryId e.entry
        EntryAmountChanged e ->
          adjustAmount (maybe (Amount 0) (.amount) $ Map.lookup e.entryId acc.entries) e.amount $
            withEntry acc e.entryId $ \entry ->
              entry {amount = e.amount}
        EntryValidated e ->
          withEntry acc e.entryId $ \entry ->
            entry {state = Validated}
        EntryCommented e ->
          withEntry acc e.entryId $ \entry ->
            entry {comment = e.comment}
        EntryMarkedInConflict e ->
          withEntry acc e.entryId $ \entry ->
            entry {state = InConflict e.reason}
        EntryDeleted e ->
          withEntries acc $
            Map.delete e.entryId
    withEntries period f = period {entries = f period.entries}
    withEntry period entryId f = withEntries period $ Map.adjust f entryId
    adjustAmount :: Amount -> Amount -> Period -> Period
    adjustAmount previous new period =
      period
        { balance = Amount $ period.balance.getAmountInCents - previous.getAmountInCents + new.getAmountInCents
        }
```

Clearly, than a lot of code, let's break this down:

* `withStartedEvent` splits the stream, extracting `Started`
* Then we `fold` over then `Event`s with `Period` initialized with `Started`'s data
* For each `Event` we have to update `Period`

The great thing with this approach is the freedom we have to define _per-query_ independent piece of code, having a fine-grain interpretation of each `Event`, meaning, the precision of our `Views` is bound to the precision of our `Event`s.
