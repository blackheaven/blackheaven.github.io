{-# LANGUAGE TemplateHaskell #-}

module InterpretersCombination where

import Control.Monad
import Data.Either
import Data.Kind
import Polysemy
-- import Polysemy.Final
import Polysemy.State

data Statement args result
  = Statement
  deriving stock (Eq, Show)

data DbError
  = DbError
  deriving stock (Eq, Show)

data Db (m :: Type -> Type) a where
  ExecQuery :: Statement args a -> args -> Db m (Either DbError a)
  Transaction :: m a -> Db m a

makeSem ''Db

data Connection
  = Connection
  deriving stock (Eq, Show)

runQuery :: Connection -> Statement args a -> args -> IO (Either DbError a)
runQuery _ _ _ = return $ Left DbError

data Pool
  = Pool
  deriving stock (Eq, Show)

runPool :: Pool -> Statement args a -> args -> IO (Either DbError a)
runPool _ _ _ = return $ Left DbError

withConnection :: Pool -> (Connection -> IO a) -> IO a
withConnection _ f = f Connection

startTransaction :: Statement () ()
startTransaction = Statement

abortTransaction :: Statement () ()
abortTransaction = Statement

commitTransaction :: Statement () ()
commitTransaction = Statement

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

-- interpretDbSingleConnectionStrategy :: forall r. Member (Final IO) r => Connection -> InterpreterFor Db r
-- interpretDbSingleConnectionStrategy c = evalState False . interpretStatefully . raiseUnder
--   where
--     interpretStatefully :: forall r'. Members '[Final IO, State Bool] r' => InterpreterFor Db r'
--     interpretStatefully =
--       withWeavingToFinal $ \_ weave _ ->
--         weave $
--           interpretFinal @IO $
--             \case
--               ExecQuery s args -> do
--                 result <- weave $ runQuery c s args
--                 -- modify (|| isLeft result)
--                 pureS result
--               Transaction t -> do
--                 error ""

-- void $ embedFinal @IO $ runQuery c startTransaction ()
-- t' <- runS t

-- result <-
--   withWeavingToFinal $ \_ weave _ ->
--     _ t'

-- -- hasError <- get
-- -- let finishStatement =
-- --       if hasError
-- --         then abortTransaction
-- --         else commitTransaction
-- -- void $ embedFinal $ runQuery c finishStatement ()

-- pure result

-- interpretDbPoolStrategy :: forall r. Member (Embed IO) r => Pool -> InterpreterFor Db r
-- interpretDbPoolStrategy pool =
--   interpretH $
--     \case
--       ExecQuery s args -> do
--         result <- embed $ runPool pool s args
--         pureT result
--       Transaction t -> do
--         t' <- runT t
--         withLowerToIO $ \lower _ ->
--           withConnection pool $ \c ->
--             lower $ raise $ interpretDbSingleConnectionStrategy c $ transaction t'
