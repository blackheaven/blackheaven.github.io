+++
title = "Polysemy: Design heuristics: Split by responsibility"
date = 2023-03-22
draft = false
path = "2023-03/polysemy-design-heuristics-srp"

[taxonomies]
categories = ["Haskell"]
tags = ["haskell", "polysemy", "design", "effects systems"]
+++

After a while, it's tempting to have large effects, just because it's easier, or because they target one implementation.

For example, in the codebases I have worked, we had one effect to deal with Users because it was targetting AWS Cognito:

```haskell
data UserEffect (m :: Type -> Type) (a :: Type) where
  SignUp :: UserInfo -> Password -> UserEffect m (Either SignUpError ())
  ConfirmSignUp :: ConfirmationInfo -> UserEffect m Bool
  SignIn :: EmailAddress -> Password -> UserEffect m (Either SignInError ())
  SignOut :: UserToken -> UserEffect m ()
  AuthenticateUser :: UserToken -> UserEffect m (Either AuthenticateUserError AuthenticatedUser)

makeSem ''UserEffect
```

While we had three concerns:

* Registration: `SignUp`, `ConfirmSignup`
* Authentication management: `SignIn`, `SignOut`
* Authentication check: `AuthenticateUser`

We can rewrite our effects as follows:

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

We can argue that `UserAuthenticationManagement` and `UserAuthenticationCheck` are closely related, but `UserAuthenticationCheck` may be used in all your server's endpoints, while `UserAuthenticationManagement` are usecases.

See the full the code [here](https://github.com/blackheaven/blackheaven.github.io/blob/master/content/code/polysemy/src/DesignHeuristicsSrp.hs).

