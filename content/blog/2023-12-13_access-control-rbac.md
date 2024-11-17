+++
title = "Access Control: Role-based Access control"
date = 2023-12-13
draft = false
path = "2023-12/access-control-rbac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

After a quick look at [Context-based Access control (CBAC)](@/blog/2023-12-10_access-control-cbac.md)
we can come back to [Access Control List (ACL)](@/blog/2023-12-06_access-control-acl.md)
types of schemes.

One big issue with ACL is the tediousness to declare permissions, you have to do
it subject by subject while it would be convenient to do it for multiples subjects
at the time.

Here comes [Role-based Access control (RBAC)](https://en.wikipedia.org/wiki/Role-based_access_control).

As its name suggests it, it's based on roles, which are assigned to subjects.

Let's introduce few types:

```haskell
data Role = SimpleUser | Admin | FileOwner deriving stock (Eq, Ord, Show)

type RbacRules role' resource action = Map.Map role' (Map.Map resource (Set.Set action))

canRbac :: (Ord role', Ord resource, Ord action) => RbacRules role' resource action -> role' -> resource -> action -> Bool
```

Actually, if we take the time to refactor our ACL implementation, they are very close:

```haskell
type AclRules resource actor action = Map.Map resource (Map.Map actor (Set.Set action))

canAcl :: (Ord resource, Ord actor, Ord action) => AclRules resource actor action -> resource -> actor -> action -> Bool
```

So close, we can simply rely on it for RBAC.

```haskell
canRbac :: (Ord role', Ord resource, Ord action) => RbacRules role' resource action -> role' -> resource -> action -> Bool
canRbac rules role resource =
  canAcl rules role resource
```

Let's add some tests:

```haskell
describe "RBAC" $ do
  let rules =
        Map.fromList
          [ ( Admin,
              Map.fromList
                [ (Motd, Set.fromList [Read, Write]),
                  (Shadow, Set.fromList [Read, Write]),
                  (FileOwned, Set.fromList [Read, Write])
                ]
            ),
            (SimpleUser, Map.fromList [(Motd, Set.fromList [Read])]),
            (FileOwner, Map.fromList [(FileOwned, Set.fromList [Read, Write])])
          ]
  forM_
    [ (SimpleUser, Motd, Read, True),
      (SimpleUser, Motd, Write, False),
      (SimpleUser, Shadow, Read, False),
      (SimpleUser, Shadow, Write, False),
      (SimpleUser, FileOwned, Read, False),
      (SimpleUser, FileOwned, Write, False),
      (Admin, Motd, Read, True),
      (Admin, Motd, Write, True),
      (Admin, Shadow, Read, True),
      (Admin, Shadow, Write, True),
      (Admin, FileOwned, Read, True),
      (Admin, FileOwned, Write, True),
      (FileOwner, FileOwned, Read, True),
      (FileOwner, FileOwned, Write, True)
    ]
    $ \tc@(role, resource, action, expected) ->
      it (show tc) $
        canRbac rules role resource action `shouldBe` expected
```

Note: `FileOwner` is a trick to represent the ownership of a file at access-time,
it's not great since it forces us to dynamically changes roles, it's not-convenient
but used in other schemes.
