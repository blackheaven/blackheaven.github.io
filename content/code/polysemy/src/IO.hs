{-# LANGUAGE TemplateHaskell #-}

module IO where

import Control.Monad.IO.Class
import Data.Kind
import GHC.Generics
import Polysemy
import Polysemy.Embed
import Polysemy.IO

data Statement args result
  = Statement
  deriving stock (Eq, Show)

data DbError
  = DbError
  deriving stock (Eq, Show)

data Db (m :: Type -> Type) a where
  ExecQuery :: Statement args a -> args -> Db m a

makeSem ''Db

newtype MonadDb a = MonadDb {runMonadDb :: IO a}
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runQuery :: Statement args a -> args -> MonadDb a
runQuery _ _ = return $ error "-"

runDb :: forall r. Member (Embed MonadDb) r => InterpreterFor Db r
runDb =
  interpret $
    \case
      ExecQuery s args -> embed $ runQuery s args

act :: forall r. Member Db r => Sem r ()
act = execQuery Statement ()

main :: IO ()
main = do
  runMonadDb $ runM $ embedToMonadIO @MonadDb $ runDb act
  runM $ lowerEmbedded runMonadDb $ runDb act
  runM $ runEmbedded runMonadDb $ runDb act
