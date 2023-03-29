+++
title = "Polysemy: Design heuristics: Effects layering"
date = 2023-03-29
draft = false
path = "2023-03/polysemy-design-heuristics-effects-layering"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

In [our previous log](@/2023-03-22_polysemy-design-heuristics-srp.md), we have seen that we had to problematics:
* Splitting effects by concerns
* Having one target implementation

A way to mitigate that is to have well-seperated, consumer-facing effects:

```haskell
data UserRegistration (m :: Type -> Type) (a :: Type) where
  SignUp :: UserInfo -> Password -> UserRegistration m (Either SignUpError ())
  ConfirmSignUp :: ConfirmationInfo -> UserRegistration m Bool

makeSem ''UserRegistration

data UserAuthenticationManagement (m :: Type -> Type) (a :: Type) where
  SignIn :: EmailAddress -> Password -> UserAuthenticationManagement m (Either SignInError ())
  SignOut :: UserToken -> UserAuthenticationManagement m ()

makeSem ''UserAuthenticationManagement

data UserAuthenticationCheck (m :: Type -> Type) (a :: Type) where
  AuthenticateUser :: UserToken -> UserAuthenticationCheck m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''UserAuthenticationCheck
```

And one internal effect:

```haskell
data InternalUserEffect (m :: Type -> Type) (a :: Type) where
  InternalSignUp :: UserInfo -> Password -> InternalUserEffect m (Either SignUpError ())
  InternalConfirmSignUp :: ConfirmationInfo -> InternalUserEffect m Bool
  InternalSignIn :: EmailAddress -> Password -> InternalUserEffect m (Either SignInError ())
  InternalSignOut :: UserToken -> InternalUserEffect m ()
  InternalAuthenticateUser :: UserToken -> InternalUserEffect m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''InternalUserEffect
```

Finally we have an interpreter per user-facing effect:

```haskell
intrepretUserRegistration :: Member InternalUserEffect r => InterpreterFor UserRegistration r
intrepretUserRegistration =
  interpret $
    \case
      SignUp userInfo password -> internalSignUp userInfo password
      ConfirmSignUp confirmationInfo -> internalConfirmSignUp confirmationInfo

intrepretUserAuthenticationManagement :: Member InternalUserEffect r => InterpreterFor UserAuthenticationManagement r
intrepretUserAuthenticationManagement =
  interpret $
    \case
      SignIn emailAddress password -> internalSignIn emailAddress password
      SignOut token -> internalSignOut token

intrepretUserAuthenticationCheck :: Member InternalUserEffect r => InterpreterFor UserAuthenticationCheck r
intrepretUserAuthenticationCheck =
  interpret $
    \case
      AuthenticateUser token -> internalAuthenticateUser token
```

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsEffetsLayering.hs).

