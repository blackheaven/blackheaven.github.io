+++
title = "Access Control: Relationship-based Access control"
date = 2023-12-27
draft = false
path = "2023-12/access-control-rebac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

Finally, here comes the scheme I have discovered and mentioned in
[the first log of the series](@/2023-12-06_access-control-acl.md):
[Relationship-based Access control](https://en.wikipedia.org/wiki/Relationship-based_access_control).

This idea is simple: it consists of finding a `Relation`, between two `Object`s.

The true power comes from the fact that the relations can be defined directly
(e.g. "Alice" "is_owner" "a_file"), or implied (e.g. all "member" "root", "can_write" "a_file").

We can start with some types:

```haskell
type ReBACRules object relationName = Map.Map relationName (Set.Set (RelationDefinition object relationName))

data RelationDefinition object relationName
  = Direct {user :: Target object relationName, object :: Target object relationName}
  | Implied {user :: Target object relationName, relation :: relationName, object :: Target object relationName}
  deriving stock (Show, Eq, Ord)

data Target object relationName
  = Object {object :: object}
  | Relation {relation :: relationName, object :: object}
  deriving stock (Show, Eq, Ord)
```

Actually I have indexed `Relation`s by `relationName` for convenience, but there
are no conceptual reason.

We can continue with few tests:

```haskell
describe "ReBAC" $ do
  let rules =
        Map.fromList
          [ ( "can_write",
              Set.fromList
                [ Direct {user = Relation {object = "doc:0", relation = "owner"}, object = Object "doc:0"},
                  Direct {user = Relation {object = "doc:1", relation = "owner"}, object = Object "doc:1"}
                ]
            ),
            ( "can_read",
              Set.fromList
                [ Direct {user = Object "user:charlie", object = Object "doc:0"},
                  Direct {user = Relation {object = "doc:0", relation = "owner"}, object = Object "doc:0"},
                  Direct {user = Relation {object = "doc:1", relation = "owner"}, object = Object "doc:1"},
                  Implied {user = Relation {object = "group:users", relation = "member"}, relation = "owner", object = Object "doc:0"} -- TODO
                  -- owners' group member
                ]
            ),
            ( "owner",
              Set.fromList
                [ Direct {user = Object "user:alice", object = Object "doc:0"},
                  Direct {user = Object "user:charlie", object = Object "doc:1"}
                ]
            ),
            ( "member",
              Set.fromList
                [ Direct {user = Object "user:alice", object = Object "group:users"},
                  Direct {user = Object "user:bob", object = Object "group:users"}
                ]
            )
          ]
  forM_
    [ ("user:alice", "can_write", "doc:0", True),
      ("user:bob", "can_write", "doc:0", False),
      ("user:charlie", "can_write", "doc:0", False),
      ("user:alice", "can_read", "doc:0", True),
      ("user:bob", "can_read", "doc:0", True),
      ("user:charlie", "can_read", "doc:0", True),
      ("user:alice", "can_write", "doc:1", False),
      ("user:bob", "can_write", "doc:1", False),
      ("user:charlie", "can_write", "doc:1", True),
      ("user:alice", "can_read", "doc:1", False),
      ("user:bob", "can_read", "doc:1", False),
      ("user:charlie", "can_read", "doc:1", True),
      ("user:charlie", "owner", "doc:1", True)
    ]
    $ \tc@(user, relation, object, expected) ->
      it (show tc) $
        canRebac rules user relation object `shouldBe` expected
```

Finally, we can do our implementation

```haskell
canRebac ::
  forall object relationName.
  (Ord relationName, Ord object) =>
  (Show relationName, Show object) =>
  ReBACRules object relationName ->
  object ->
  relationName ->
  object ->
  Bool
canRebac rules = forRelation
  where
    forRelation user relation object =
      any (withRelation user object) $
        Map.findWithDefault mempty relation rules
    withRelation user object =
      \case
        r@(Direct {}) ->
          Set.member user (objects r.user) && Set.member object (objects r.object)
        r@(Implied {}) ->
          or
            [ forRelation user' r.relation object
              | user' <- Set.toList (objects r.user)
            ]
    objects :: Target object relationName -> Set.Set object
    objects =
      \case
        o@(Object {}) -> Set.singleton o.object
        o@(Relation {}) -> foldMap go $ Map.findWithDefault mempty o.relation rules
          where
            go =
              \case
                r@(Direct {}) ->
                  whenTargettedObject r.object $
                    objects r.user
                r@(Implied {}) ->
                  whenTargettedObject r.object $
                    Set.fromList
                      [ user
                        | user <- Set.toList (objects r.user),
                          object <- Set.toList (objects r.object),
                          forRelation user r.relation object
                      ]
            whenTargettedObject objs xs =
              if Set.member o.object (objects objs)
                then xs
                else mempty
```


Well, it does not look simpler in the end, we can break it down:

* `forRelation` determines whether there is a relation between two objects by listing all `RelationDefinition`s for a `relationName` and checking them one by one
* `withRelation` checks if a `RelationDefinition` is between the two given objects (here's where we have to explore transitive `Relation`s for `Implied` `Relation`s)
* `objects` transitively lists objects (on both sides) of a `Relation`

That's a solid beginning, if you want to go further [OpenFGA](https://openfga.dev),
a ReBAC implementation has a good [modeling tutorial](https://openfga.dev/docs/modeling/getting-started).

Moreover it has some interesting features:

* Types (e.g. `user:Alice`, `group:users`, etc.)
* Wildcards rules (e.g. `group:*`)
