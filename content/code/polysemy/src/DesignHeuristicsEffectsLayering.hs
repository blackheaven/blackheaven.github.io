{-# LANGUAGE TemplateHaskell #-}

module DesignHeuristicsEffectsLayering where

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

data InternalUserEffect (m :: Type -> Type) (a :: Type) where
  InternalSignUp :: UserInfo -> Password -> InternalUserEffect m (Either SignUpError ())
  InternalConfirmSignUp :: ConfirmationInfo -> InternalUserEffect m Bool
  InternalSignIn :: EmailAddress -> Password -> InternalUserEffect m (Either SignInError ())
  InternalSignOut :: UserToken -> InternalUserEffect m ()
  InternalAuthenticateUser :: UserToken -> InternalUserEffect m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''InternalUserEffect

interpretInternalUserEffectCognito :: InterpreterFor InternalUserEffect r
interpretInternalUserEffectCognito = error "todo"

-- Cleaner
data UserRegistration (m :: Type -> Type) (a :: Type) where
  SignUp :: UserInfo -> Password -> UserRegistration m (Either SignUpError ())
  ConfirmSignUp :: ConfirmationInfo -> UserRegistration m Bool

makeSem ''UserRegistration

intrepretUserRegistration :: Member InternalUserEffect r => InterpreterFor UserRegistration r
intrepretUserRegistration =
  interpret $
    \case
      SignUp userInfo password -> internalSignUp userInfo password
      ConfirmSignUp confirmationInfo -> internalConfirmSignUp confirmationInfo

data UserAuthenticationManagement (m :: Type -> Type) (a :: Type) where
  SignIn :: EmailAddress -> Password -> UserAuthenticationManagement m (Either SignInError ())
  SignOut :: UserToken -> UserAuthenticationManagement m ()

makeSem ''UserAuthenticationManagement

intrepretUserAuthenticationManagement :: Member InternalUserEffect r => InterpreterFor UserAuthenticationManagement r
intrepretUserAuthenticationManagement =
  interpret $
    \case
      SignIn emailAddress password -> internalSignIn emailAddress password
      SignOut token -> internalSignOut token

data UserAuthenticationCheck (m :: Type -> Type) (a :: Type) where
  AuthenticateUser :: UserToken -> UserAuthenticationCheck m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''UserAuthenticationCheck

intrepretUserAuthenticationCheck :: Member InternalUserEffect r => InterpreterFor UserAuthenticationCheck r
intrepretUserAuthenticationCheck =
  interpret $
    \case
      AuthenticateUser token -> internalAuthenticateUser token

interpretAllUserEffectsCognito :: InterpretersFor '[UserRegistration, UserAuthenticationManagement, UserAuthenticationCheck] r
interpretAllUserEffectsCognito =
  interpretInternalUserEffectCognito
    . intrepretUserAuthenticationCheck
    . intrepretUserAuthenticationManagement
    . intrepretUserRegistration
    . raise3Under @InternalUserEffect
