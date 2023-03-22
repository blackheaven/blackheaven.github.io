{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsSrp where

import Data.Kind
import Polysemy

data UserInfo

data Password

data SignUpError

data ConfirmationInfo

data EmailAddress

data SignInError

data UserToken

data AuthenticateUserError

data AuthenticatedUser

data UserEffect (m :: Type -> Type) (a :: Type) where
  SignUp :: UserInfo -> Password -> UserEffect m (Either SignUpError ())
  ConfirmSignUp :: ConfirmationInfo -> UserEffect m Bool
  SignIn :: EmailAddress -> Password -> UserEffect m (Either SignInError ())
  SignOut :: UserToken -> UserEffect m ()
  AuthenticateUser :: UserToken -> UserEffect m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''UserEffect

-- Cleaner
data UserRegistration (m :: Type -> Type) (a :: Type) where
  SignUp' :: UserInfo -> Password -> UserRegistration m (Either SignUpError ())
  ConfirmSignUp' :: ConfirmationInfo -> UserRegistration m Bool

makeSem ''UserRegistration

data UserAuthenticationManagement (m :: Type -> Type) (a :: Type) where
  SignIn' :: EmailAddress -> Password -> UserAuthenticationManagement m (Either SignInError ())
  SignOut' :: UserToken -> UserAuthenticationManagement m ()

makeSem ''UserAuthenticationManagement

data UserAuthenticationCheck (m :: Type -> Type) (a :: Type) where
  AuthenticateUser' :: UserToken -> UserAuthenticationCheck m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''UserAuthenticationCheck
