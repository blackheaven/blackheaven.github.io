+++
title = "Access Control: Context-based Access control"
date = 2023-12-10
draft = false
path = "2023-12/access-control-cbac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

We have started our series with [Access Control List (ACL)](@/blog/2023-12-06_access-control-acl.md),
the good news is that it's the basis for the most of the access control schemes,
however they are usually stateless.

The thing is it cannot work in cases the system should be in a certain state
to perform some operation, that's the case with firewalls trying to check protocols.

Here come [Context-based Access control (CBAC)](https://en.wikipedia.org/wiki/Context-based_access_control).

The mechanism is the following: each time a new packet come we check the type of
packet against the state and if it's one of the allowed transition we can
update it.

Let's try with TCP.

As a reminder, TCP works as follows:

* `SYN` (handshake)
* `SYN/ACK` (handshake)
* `ACK` (handshake)
* Data exchange

We can draft the following types:

```haskell
data TcpState
  = NotInitialized
  | HandShakeSyned 
  | HandShakeSynedAcked
  | Opened
  deriving stock (Eq, Ord, Enum, Show)

data TcpAction
  = Syn
  | SynAck
  | Ack
  | SendData
  | Close
  deriving stock (Eq, Ord, Enum, Show)

canCbac :: TcpState -> TcpAction -> Maybe TcpState
```

We can come up with some tests:

```haskell
    describe "CBAC"
      $ forM_
        [ (NotInitialized, Syn, Just HandShakeSyned),
          (NotInitialized, SynAck, Nothing),
          (NotInitialized, Ack, Nothing),
          (NotInitialized, SendData, Nothing),
          (NotInitialized, Close, Nothing),
          (HandShakeSyned, Syn, Nothing),
          (HandShakeSyned, SynAck, Just HandShakeSynedAcked),
          (HandShakeSyned, Ack, Nothing),
          (HandShakeSyned, SendData, Nothing),
          (HandShakeSyned, Close, Nothing),
          (HandShakeSynedAcked, Syn, Nothing),
          (HandShakeSynedAcked, SynAck, Nothing),
          (HandShakeSynedAcked, Ack, Just Opened),
          (HandShakeSynedAcked, SendData, Nothing),
          (HandShakeSynedAcked, Close, Nothing),
          (Opened, Syn, Nothing),
          (Opened, SynAck, Nothing),
          (Opened, Ack, Nothing),
          (Opened, SendData, Just Opened),
          (Opened, Close, Just NotInitialized)
        ]
      $ \tc@(state, action, expected) ->
        it (show tc) $
          canCbac state action `shouldBe` expected
```

Finally we have a classic state-machine updater function:

```haskell
canCbac :: TcpState -> TcpAction -> Maybe TcpState
canCbac state action =
  case state of
    NotInitialized -> Syn `transitionTo` HandShakeSyned
    HandShakeSyned -> SynAck `transitionTo` HandShakeSynedAcked
    HandShakeSynedAcked -> Ack `transitionTo` Opened
    Opened -> SendData `transitionTo` Opened <|> Close `transitionTo` NotInitialized
  where
    transitionTo onAction newState =
      if action == onAction
        then Just newState
        else Nothing
```
