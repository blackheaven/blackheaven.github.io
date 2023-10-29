{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module BadMonad where

import Control.Monad.Reader (ReaderT, asks)
import Polysemy
import Polysemy.Reader (Reader, ask, runReader)

data SmtpConfig = SmtpConfig

data S3Config = S3Config

data RedisParams = RedisParams

class (Monad m) => MyAppMonad m where
  getSmtpConfig :: m SmtpConfig
  getS3Config :: m S3Config
  getRedisParams :: m RedisParams

newtype MyAppM a = MyAppM {runMyAppM :: ReaderT AppContext IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)

data AppContext = AppContext
  { acSmtpConfig :: SmtpConfig,
    acRedisParams :: RedisParams,
    acS3Config :: S3Config
  }

instance MyAppMonad MyAppM where
  getSmtpConfig = MyAppM $ asks acSmtpConfig
  getS3Config = MyAppM $ asks acS3Config
  getRedisParams = MyAppM $ asks acRedisParams

fetchUser :: (MyAppMonad m) => UserName -> m (Maybe User)
fetchUser _ = error "TODO"

storeAvatar :: (MyAppMonad m) => UserId -> ByteString -> m ()
storeAvatar _ _ = error "TODO"

notifySignUp :: (MyAppMonad m) => UserId -> PassCode -> m ()
notifySignUp _ _ = error "TODO"

data UserId

data UserName

data ByteString

data PassCode

data User

class MyAppMonad' m where
  fetchUser' :: UserName -> m (Maybe User)
  storeAvatar' :: UserId -> ByteString -> m ()
  notifySignUp' :: UserId -> PassCode -> m ()

newtype NoopAppM a = NoopAppM {runNoopAppM :: IO a}
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance MyAppMonad' NoopAppM where
  fetchUser' _ = NoopAppM $ return Nothing
  storeAvatar' _ _ = NoopAppM $ return ()
  notifySignUp' _ _ = NoopAppM $ return ()

type MyAppMonadEffects =
  '[ Reader SmtpConfig,
     Reader S3Config,
     Reader RedisParams
   ]

type MyAppMonad'' m = m ~ Sem MyAppMonadEffects

runMyAppMonad'' :: AppContext -> Sem MyAppMonadEffects a -> a
runMyAppMonad'' ctx =
  run
    . runReader ctx.acRedisParams
    . runReader ctx.acS3Config
    . runReader ctx.acSmtpConfig

getSmtpConfig'' :: (Member (Reader SmtpConfig) r) => Sem r SmtpConfig
getSmtpConfig'' = ask

getS3Config'' :: (Member (Reader S3Config) r) => Sem r S3Config
getS3Config'' = ask

getRedisParams'' :: (Member (Reader RedisParams) r) => Sem r RedisParams
getRedisParams'' = ask

fetchUser'' :: (MyAppMonad'' m) => UserName -> m (Maybe User)
fetchUser'' _ = error "TODO"

storeAvatar'' :: (MyAppMonad'' m) => UserId -> ByteString -> m ()
storeAvatar'' _ _ = error "TODO"

notifySignUp'' :: (MyAppMonad'' m) => UserId -> PassCode -> m ()
notifySignUp'' _ _ = error "TODO"
