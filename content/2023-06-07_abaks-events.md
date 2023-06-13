+++
title = "Abaks: events"
date = 2023-06-07
draft = false
path = "2023-06/abaks-events"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

I tend to start with the events whenever I start an event source-based system.

But first, let's recall the requirements:

> * I should start a _Period_ with a _name_ (i.e. "June 2023"), start and stop dates, an _initial balance_
> * I can add/change/comment/delete an _Entry_, which is defined by an _amount_, a _date_, and a category
> * An _Entry_ can be expected or not
> * At the end, I should be able to validate or flag _in conflict_ an entry

Which gives us:

```haskell
data AbaksEvent
  = Started {periodId :: PeriodId, name :: Text, from :: Day, to :: Day, initialBalance :: Amount}
  | EntryAdded {entry :: Entry}
  | EntryAmountChanged {entryId :: EntryId, amount :: Amount}
  | EntryValidated {entryId :: EntryId}
  | EntryCommented {entryId :: EntryId, comment :: Text}
  | EntryMarkedInConflict {entryId :: EntryId, reason :: Text}
  | EntryDeleted {entryId :: EntryId, comment :: Text}
  deriving stock (Eq, Show, Generic)
```

Also with the following supporting types:

```haskell
newtype PeriodId = PeriodId {getPeriodId :: Int}
  deriving stock (Eq, Ord, Show, Generic)

data Entry = Entry
  { entryId :: EntryId,
    amount :: Amount,
    category :: Text,
    comment :: Text,
    state :: EntryState,
    date :: Day
  }
  deriving stock (Eq, Show, Generic)

newtype EntryId = EntryId {getEntryId :: Int}
  deriving stock (Eq, Ord, Show, Generic)

newtype Amount = Amount {getAmountInCents :: Int}
  deriving stock (Eq, Ord, Show, Generic)

data EntryState
  = Expected
  | Unexpected
  | Validated
  | InConflict Text
  deriving stock (Eq, Show, Generic)
```

Actually, there's four shortcomings in this design:

* `EntryAmountChanged` is not explicitly required and might probably be a parameter of `EntryMarkedInConflict`
* `EntryDeleted` is not required
* `EntryAdded` takes a full `Entry`, which mean it could be in a 'final' `EntryState`
* `Amount` does not distinguish deposits and withdrawals

I would argue that these are 'improvements' made for UX purposes, users can make mistakes, enter the wrong amount, or on the wrong period, etc.

Next time we will see the commands, we'll have a lot to cover since I have a quite opinionated design.
