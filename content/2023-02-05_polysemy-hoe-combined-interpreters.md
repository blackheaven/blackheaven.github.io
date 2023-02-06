+++
title = "Polysemy: Higher order Effects and combined interpreters"
date = 2023-02-05
draft = false
path = "2023-02/polysemy-hoe-combined-interpreters"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

[In a previous log](@/2023-01-25_polysemy-hoe-tactics-binding.md), we saw that we are able to call other interpreters during interpretations.

```haskell
interpretBindTactic :: InterpreterFor BindE r
interpretBindTactic =
  interpretH $
    \case
      BindE f g -> do
        ma <- runT f
        mf <- bindT g
        let runHoE = raise . interpretBindTactic
        runHoE ma >>= runHoE . mf
```

It could be particularly useful when you have context changing higher-order effects.

Let's image a relational database effect:

```haskell
data Db (m :: Type -> Type) a where
  ExecQuery :: Statement args a -> args -> Db m (Either DbError a)
  Transaction :: m a -> Db m a

makeSem ''Db
```

If we provide a single connection-interpreter everything works as transaction works for a connections:

```haskell
interpretDbSingleConnectionTactic :: forall r. Member (Embed IO) r => Connection -> InterpreterFor Db r
interpretDbSingleConnectionTactic c = evalState False . interpretStatefully . raiseUnder
  where
    interpretStatefully :: InterpreterFor Db (State Bool ': r)
    interpretStatefully =
      interpretH $
        \case
          ExecQuery s args -> do
            result <- embed $ runQuery c s args
            modify (|| isLeft result)
            pureT result
          Transaction t -> do
            void $ embed $ runQuery c startTransaction ()
            t' <- runT t

            result <-
              withLowerToIO $ \lower _ -> do
                let toIO :: Sem (Db ': State Bool ': r) x -> IO x
                    toIO = lower . raise . interpretStatefully
                toIO t'
            hasError <- get
            let finishStatement =
                  if hasError
                    then abortTransaction
                    else commitTransaction
            void $ embed $ runQuery c finishStatement ()

            pure result
```

However, if we a `Pool`, we should run the transaction over one connection, so, we should call the other single connection interpreter:

```haskell
interpretDbPoolTactic :: forall r. Member (Embed IO) r => Pool -> InterpreterFor Db r
interpretDbPoolTactic pool =
  interpretH $
    \case
      ExecQuery s args -> do
        result <- embed $ runPool pool s args
        pureT result
      Transaction t -> do
        t' <- runT t
        withLowerToIO $ \lower _ ->
          withConnection pool $ \c ->
            lower $ raise $ interpretDbSingleConnectionTactic c $ transaction t'
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/InterpretersCombination.hs).
