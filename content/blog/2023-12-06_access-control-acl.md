+++
title = "Access Control: ACL"
date = 2023-12-06
draft = false
path = "2023-12/access-control-acl"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

Few days ago I was watching again [Welcome to Zanzibar](https://www.youtube.com/watch?v=FtPTMLJWgp4)
a French talk about authorization which was given at [XCraft](https://xcraft.fr/),
an event organized early October by the [Software Crafters Lyon](https://www.meetup.com/fr-FR/software-craftsmanship-lyon/),
which I'm part of.

I realized that I was not really remembering *all* the authorization schemes,
and I thought starting a new series on them could be a good idea.

Let's start with a distinction:

* Authentication: determines *who* the actor is
* Authorization: determines *what* can be done

You can perfectly be authenticated but don't deal with permissions, conversely
you can authorize action without definite actor.

Let start with [Access-Control list (ACL)](https://en.wikipedia.org/wiki/Access-control_list),
which is the most basic scheme.

The idea is to determine whether an *actor* can perform an *action* on a *resource*.

Let's take an UNIX-derived Operating System (any GNU/Linux distribution, FreeBSD,
OpenBSD, NetBSD, etc.), they use this mechanism, especially for file access.

Let's start with some general types:

```haskell
data Resource = Motd | Shadow deriving stock (Eq, Ord, Show)

data Actor = Alice | Bob deriving stock (Eq, Ord, Show)

data Action = Read | Write deriving stock (Eq, Ord, Show)
```

* `Actor` represent system users
* `Resource` are actual file (`motd` is the file containing the welcome message when you `ssh` into a machine, `shadow` contains users' password)

The algorithm is pretty simple:

> For any `Resource`, an `Actor` should have a set of authorized `Action`

We can draft an implementation:

```haskell
canAcl :: AclRules -> Resource -> Actor -> Action -> Bool
canAcl rules resource actor action =
  Set.member action $
    Map.findWithDefault mempty actor $
      Map.findWithDefault mempty resource rules
```

The main idea is: each time a lookup fail, default on an empty container, so we
end-up with an empty set of permission and any permission queried will fail.

To make things simple, I have a type alias as `AclRules`:

```haskell
type AclRules = Map.Map Resource (Map.Map Actor (Set.Set Action))
```

Finally, here are some rules and tests:

```haskell
let rules =
      Map.fromList
        [ ( Motd,
            Map.fromList
              [ (Alice, Set.fromList [Read]),
                (Bob, Set.fromList [Read, Write])
              ]
          ),
          (Shadow, Map.fromList [(Bob, Set.fromList [Read, Write])])
        ]
forM_
  [ (Alice, Motd, Read, True),
    (Alice, Motd, Write, False),
    (Alice, Shadow, Read, False),
    (Alice, Shadow, Write, False),
    (Bob, Motd, Read, True),
    (Bob, Motd, Write, True),
    (Bob, Shadow, Read, True),
    (Bob, Shadow, Write, True)
  ]
  $ \tc@(actor, resource, action, expected) ->
    it (show tc) $
      canAcl rules resource actor action `shouldBe` expected
```
