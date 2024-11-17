+++
title = "Access Control: Attribute-based Access control"
date = 2023-12-17
draft = false
path = "2023-12/access-control-abac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

Previously we have seen [Role-based Access control](@/blog/2023-12-13_access-control-rbac.md),
we have seen it has two shortcomings:

* We have to create one role per access
* We have to dynamically assign roles if we want to deal with ownership

Here comes [Attribute-based Access control (ABAC)](https://en.wikipedia.org/wiki/Attribute-based_access_control).

The idea is simple, instead of an identifier (a role or a subject) and a target
(a resource), we give a set of attributes (resource/role/subject/context information)
and an expression rule.

Note: the context can be a lot of things such as time of system state.

Note 2 : it is the mechanism used in AWS Identity and Access Management (IAM) policy definition

Let's start with some types:

```haskell
data Attribute
  = Role Role
  | Actor Actor
  | Resource Resource
  | Action Action
  | Arbitrary Text Text
  deriving stock (Eq, Ord, Show)

data AbacRules
  = Or AbacRules AbacRules
  | And AbacRules AbacRules
  | Has Attribute
  | Match (Attribute -> Attribute -> Bool)

canAbac :: AbacRules -> Set.Set Attribute -> Bool
```

`Match` our trick to deal with ownership as it helps us to zip the attributes-set
on it-self, so we can define it as:

```haskell
isOwner :: AbacRules
isOwner =
  Match $ \x y ->
    case (x, y) of
      (Actor actor, Resource (FileOwnBy owner)) -> actor == owner
      _ -> False
```

We can exercise it with some tests:

```haskell
  describe "ABAC" $ do
    let rules =
          Has (Role Admin)
            `Or` isOwner
            `Or` (Has (Resource Motd) `And` Has (Action Read))
    forM_
      [ ([Resource Motd, Action Read, Actor Alice, Role SimpleUser], True),
        ([Resource Motd, Action Write, Actor Alice, Role SimpleUser], False),
        ([Resource Motd, Action Write, Actor Bob, Role Admin], True),
        ([Resource (FileOwnBy Alice), Actor Alice], True),
        ([Resource (FileOwnBy Alice), Actor Charlie], False)
      ]
      $ \tc@(attributes, expected) ->
        it (show tc) $
          canAbac rules (Set.fromList attributes) `shouldBe` expected
```

And finally the implementation:

```haskell
canAbac :: AbacRules -> Set.Set Attribute -> Bool
canAbac rules attributes =
  case rules of
    Or x y -> canAbac x attributes || canAbac y attributes
    And x y -> canAbac x attributes && canAbac y attributes
    Has x -> Set.member x attributes
    Match p -> or [p x y | x <- Set.toList attributes, y <- Set.toList attributes]
```
