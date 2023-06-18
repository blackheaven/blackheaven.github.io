+++
title = "Abaks: Use Cases"
date = 2023-06-16
draft = false
path = "2023-06/abaks-use-cases"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

After introducing [`Command`s](@/2023-06-11_abaks-commands.md) as our _Entites_/_Core Domain_, we then have to introduce next layer: _Use Cases_.

For reference, it is the place where application-specific wiring.

They aim to be really simple, most of them will just wrap _Event sourcing_ mechanics:

```haskell
addEntry ::
  Members '[EventSourceEffect Entities.AbaksEvent] r =>
  Entities.PeriodId ->
  Entities.Entry ->
  Sem r (Either Text ())
addEntry periodId entry =
  runCommand (Entities.addEntry entry) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const ())
```

While some of them will inject some value (or connect various sources):

```haskell
createPeriod ::
  Members '[EventSourceEffect Entities.AbaksEvent, Final IO] r =>
  Text ->
  Day ->
  Day ->
  Entities.Amount ->
  Sem r (Either Text Entities.PeriodId)
createPeriod name from to balance = do
  periodId <- Entities.PeriodId . AggregateId . UUID.toText <$> embedFinal UUID.nextRandom
  runCommand (Entities.startPeriod periodId name from to balance) periodId.getPeriodId $
    return . bimap (.getExplainedError) (const periodId)
```

In the meantime, we have created a supporting function:

```haskell
runCommand ::
  Members '[EventSourceEffect eventType] r =>
  CommandHandler eventType e ->
  AggregateId ->
  (Either e (Events eventType) -> Sem r a) ->
  Sem r a
runCommand handler aggregateId f =
  withEvents aggregateId $ \initialEvents ->
    let result = applyCommand handler initialEvents
     in (,) (either mempty id result) <$> f result
```

And a dedicated effect:

```haskell
data EventSourceEffect (eventType :: Type) (m :: Type -> Type) (a :: Type) where
  WithEvents :: AggregateId -> (Events eventType -> m (Events eventType, a)) -> EventSourceEffect eventType m a
```

We also provide two implementations:

* A pure in-memory, `State`-based
* A file-based, backed by a `MVar`, relying on `Aeson` (which obviously won't scale, or even be maintainable, but good enough for the moment)

All based on a simple type: `Map.Map AggregateId (Events eventType)`
