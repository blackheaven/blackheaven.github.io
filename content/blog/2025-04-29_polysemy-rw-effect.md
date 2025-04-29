+++
title = "Polysemy: RW effects"
date = 2025-04-29
draft = false
path = "2025-04/polysemy-rw-effect"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

I haven't written on [polysemy](https://hackage.haskell.org/package/polysemy)
in a while, let's change that.

Previously, when I had to create effects on a same topic, but with different
access patterns, I would create distinct effects/interpreters:

```haskell
data SomethingWrite (m :: Type -> Type) a where
  Write :: SomethingWrite m ()

makeSem ''SomethingWrite

data SomethingRead (m :: Type -> Type) a where
  Read :: SomethingRead m ()

makeSem ''SomethingRead

runSomethingWrite :: InterpreterFor SomethingWrite r
runSomethingWrite =
  interpret $
    \case
      Write -> return ()

runSomethingRead :: InterpreterFor SomethingRead r
runSomethingRead =
  interpret $
    \case
      Read -> return ()

consumerWrite :: Members '[SomethingWrite] r => Sem r ()
consumerWrite = write

consumerRead :: Members '[SomethingRead] r => Sem r ()
consumerRead = read

consumerBoth :: Members '[SomethingRead, SomethingWrite] r => Sem r ()
consumerBoth = consumerWrite >> consumerRead

runConsumers :: ()
runConsumers =
  run $
    runSomethingWrite $
      runSomethingRead consumerBoth
```

Alternatively, we could define one, but add a phantom type, which has a
constraint:

```haskell
data AccessLevel = R | W | RW

type family CanRead accessLevel :: Constraint where
  CanRead 'R = ()
  CanRead 'RW = ()

type family CanWrite accessLevel :: Constraint where
  CanWrite 'W = ()
  CanWrite 'RW = ()

data Something (accessLevel :: AccessLevel) (m :: Type -> Type) a where
  Write' :: forall accessLevel m. CanWrite accessLevel => Something accessLevel m ()
  Read' :: forall accessLevel m. CanRead accessLevel => Something accessLevel m ()

makeSem ''Something

runSomethingRW :: InterpreterFor (Something 'RW) r
runSomethingRW =
  interpret $
    \case
      Read' -> return ()
      Write' -> return ()
```

We could safely define a "partial" interpreter:

```haskell
runSomethingR :: InterpreterFor (Something 'R) r
runSomethingR =
  interpret $
    \case
      Read' -> return ()
```

Note: GHC will emit an incompleteness alter because Haskell constraints do not
carry closeness.

We can then use this new implementation, composing constraints:

```haskell
consumerWrite' :: (CanWrite accessLevel, Members '[Something accessLevel] r) => Sem r ()
consumerWrite' = write'

consumerRead' :: (CanRead accessLevel, Members '[Something accessLevel] r) => Sem r ()
consumerRead' = read'

consumerBoth' :: (CanRead accessLevel, CanWrite accessLevel, Members '[Something accessLevel] r) => Sem r ()
consumerBoth' = consumerWrite' >> consumerRead'

runConsumers' :: ()
runConsumers' =
  run $
    runSomethingRW consumerBoth'
```
