+++
title = "Abaks: commands"
date = 2023-06-11
draft = false
path = "2023-06/abaks-commands"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

[Previous log](@/blog/2023-06-07_abaks-events.md) has let us with the following events:

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

In order to emit these events, we needs `Command`s (or `CommandHandler`s) which will eventually create new `Event`s.

Let's start with some structuring definitions regarding `Command`s handling:

```haskell
type CommandHandler a e = Events a -> Either e (Events a)

type Events a = [a]

applyCommand ::
  CommandHandler a e ->
  Events a ->
  Either e (Events a)
applyCommand = ($)
```

Here's is the controversy.
Whenever I talk to other event sourcing practitioners (mostly coming from OOP/OOD world), I get the following feedback regarding my not-compliant design:

1. You should model your aggregate, and apply `Command`s against them
2. `Command`s/`CommandHandler`s are not supposed to have '`return`' values
3. You should have some kind of `IO` to generate some value (e.g. UUIDs generation)

Here are my usual answers:

1. I don't actually need an aggregate, there's multiple ways to look at it
  1. Your aggregate is just a projection of the `Event`s, so you can shortcut the whole process
  2. Your aggregate computation can be buggy, without aggregate, you'll have less code, so less bugs
  3. You can have a fine-grains logic with `Event`s you won't be able to have without effort in an aggregate
  4. An aggregate is design to answer all questions, while you can factor-out precise ones
2. There's two parts in this topic
  1. These are just a `Command`s/`CommandHandler`s, not the complete event sourcing system, which tends to produce no value (even though they use to shamelessly throw exceptions). Moreover, nothing should prevent you to emit `Event`s when things go wrong.
  2. While I agree on the principle to put CQRS first, having some kind of `Reactor` which would push feedback to the user from `Event`s interpretation, I find that to be an unnecessarily complex default design.
3. This one is shocking to me, the last thing you want is to have two identical (same aggregates, same `Event`s, same `Command`s' values) `Command`s have different behaviors. For UUIDs, let's imagine you're unlucky (picking and existing one), either don't handle the case, or provide alternative value.

Note: sometimes I also hear that, without aggregate, you cannot show the code to the business people. I have two issues with that:

1. I don't really understand how a multiple part (aggregate-based) piece of code is easier to understand than an `Event`s-based one (which will be closer than an event storming session)
2. Just don't show the code to the business, period. A while ago, I had a friend which was studying to be top manager/C-Level in hospitals. One of her lesson was called "database modeling". She showed me a test she had to take, I did not even understood what they were asking. So, I could start by saying that it's not business job to understand the very small details of our work, but it's our job to articulated what we are doing. But instead, I would argue that I won't lower my code quality (not using relevant features, or adding obvious-but-to-be-maintained comments) or make the code harder-than-necessary to work with (renaming standard functions, be not idiomatic, which will anyway, make on-boarding harder and communication with the business harder), for people not supposed to directly work on it.

Then we can have a look at our `CommandHandler`s:

```haskell
startPeriod :: PeriodId -> Text -> Day -> Day -> Amount -> CommandHandler AbaksEvent ExplainedError
startPeriod periodId name from to balance events = do
  unless (null events) $
    Left "Period already started"
  return
    [ Started
        { periodId = periodId,
          name = name,
          from = from,
          to = to,
          initialBalance = balance
        }
    ]

addEntry :: Entry -> CommandHandler AbaksEvent ExplainedError
addEntry entry events = do
  hasStarted events
  inPeriod entry.date events
  let entries = listEntries events
  unless (Map.notMember entry.entryId entries) $
    Left "Entry already existing"
  return [EntryAdded entry]

changeAmountEntry :: EntryId -> Amount -> CommandHandler AbaksEvent ExplainedError
changeAmountEntry entryId amount events = do
  validEntry entryId events
  return [EntryAmountChanged entryId amount]

validateEntry :: EntryId -> CommandHandler AbaksEvent ExplainedError
validateEntry entryId events = do
  validEntry entryId events
  return [EntryValidated entryId]

commentEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
commentEntry entryId comment events = do
  validEntry entryId events
  return [EntryCommented entryId comment]

markInClonflictEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
markInClonflictEntry entryId reason events = do
  validEntry entryId events
  return [EntryMarkedInConflict entryId reason]

deleteEntry :: EntryId -> Text -> CommandHandler AbaksEvent ExplainedError
deleteEntry entryId comment events = do
  validEntry entryId events
  return [EntryDeleted entryId comment]
```

And we have our supporting functions:

```haskell
newtype ExplainedError = ExplainedError {getExplainedError :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

hasStarted :: Events AbaksEvent -> Either ExplainedError ()
hasStarted =
  \case
    (Started {} : _) -> return ()
    _ -> Left "Period is not properly defined"

inPeriod :: Day -> Events AbaksEvent -> Either ExplainedError ()
inPeriod x =
  \case
    (Started {..} : _) ->
      if x >= from && x <= to
        then return ()
        else Left "Out of period"
    _ -> Left "Period is not properly defined"

listEntries :: Events AbaksEvent -> Map.Map EntryId (Either () Entry)
listEntries = foldl' go mempty
  where
    go :: Map.Map EntryId (Either () Entry) -> AbaksEvent -> Map.Map EntryId (Either () Entry)
    go entries =
      \case
        Started {} -> entries
        EntryAdded x -> Map.insert x.entryId (Right x) entries
        EntryAmountChanged {} -> entries
        EntryValidated {} -> entries
        EntryCommented {} -> entries
        EntryMarkedInConflict {} -> entries
        EntryDeleted {..} -> Map.insert entryId (Left ()) entries

validEntry :: EntryId -> Events AbaksEvent -> Either ExplainedError ()
validEntry entryId events = do
  let entries = listEntries events
  case Map.lookup entryId entries of
    Nothing -> Left "Unknown entry"
    Just (Left ()) -> Left "Deleted entry"
    Just (Right _) -> Right ()
```

Next time we'll briefly talk about testing.
