{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Spec (main, spec) where

import Control.Applicative (liftA3, (<|>))
import Control.Monad (forM_)
import Data.List (unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import GHC.Exts (IsString)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ACL" $ do
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

-- Generic
data Resource = Motd | Shadow | FileOwned | FileOwnBy Actor deriving stock (Eq, Ord, Show)

data Actor = Alice | Bob | Charlie deriving stock (Eq, Ord, Show)

data Action = Read | Write deriving stock (Eq, Ord, Show)

-- | ACL
-- \* https://en.wikipedia.org/wiki/Access-control_list
type AclRules resource actor action = Map.Map resource (Map.Map actor (Set.Set action))

canAcl :: (Ord resource, Ord actor, Ord action) => AclRules resource actor action -> resource -> actor -> action -> Bool
canAcl rules resource actor action =
  Set.member action $
    Map.findWithDefault mempty actor $
      Map.findWithDefault mempty resource rules

-- | CBAC
-- * https://en.wikipedia.org/wiki/Context-based_access_control
-- * https://www.geeksforgeeks.org/context-based-access-control-cbac/
data TcpState = NotInitialized | HandShakeSyned | HandShakeSynedAcked | Opened deriving stock (Eq, Ord, Enum, Show)

data TcpAction = Syn | SynAck | Ack | SendData | Close deriving stock (Eq, Ord, Enum, Show)

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

-- | RBAC
-- * https://en.wikipedia.org/wiki/Role-based_access_control
-- * https://www.okta.com/identity-101/role-based-access-control-vs-attribute-based-access-control/
data Role = SimpleUser | Admin | FileOwner deriving stock (Eq, Ord, Show)

type RbacRules role' resource action = Map.Map role' (Map.Map resource (Set.Set action))

canRbac :: (Ord role', Ord resource, Ord action) => RbacRules role' resource action -> role' -> resource -> action -> Bool
canRbac rules role resource =
  canAcl rules role resource

-- | ABAC
-- * https://en.wikipedia.org/wiki/Attribute-based_access_control
-- * https://www.okta.com/blog/2020/09/attribute-based-access-control-abac/
-- * https://www.okta.com/identity-101/role-based-access-control-vs-attribute-based-access-control/
data Attribute = Role Role | Actor Actor | Resource Resource | Action Action | Arbitrary Text Text deriving stock (Eq, Ord, Show)

data AbacRules
  = Or AbacRules AbacRules
  | And AbacRules AbacRules
  | Has Attribute
  | Match (Attribute -> Attribute -> Bool)

canAbac :: AbacRules -> Set.Set Attribute -> Bool
canAbac rules attributes =
  case rules of
    Or x y -> canAbac x attributes || canAbac y attributes
    And x y -> canAbac x attributes && canAbac y attributes
    Has x -> Set.member x attributes
    Match p -> or [p x y | x <- Set.toList attributes, y <- Set.toList attributes]

isOwner :: AbacRules
isOwner =
  Match $ \x y ->
    case (x, y) of
      (Actor actor, Resource (FileOwnBy owner)) -> actor == owner
      _ -> False

-- | GBAC
-- * https://en.wikipedia.org/wiki/Graph-based_access_control
type GbacDepartmentRules functionalUnit resource action = Map.Map functionalUnit (Map.Map resource (Set.Set action))

type Staff functionalUnit actor =
  Map.Map actor functionalUnit

type Delegations functionalUnit actor =
  Map.Map (Either functionalUnit actor) (Set.Set (Either functionalUnit actor))

canGbac ::
  forall functionalUnit resource actor action.
  (Ord functionalUnit, Ord resource, Ord action, Ord actor) =>
  GbacDepartmentRules functionalUnit resource action ->
  Staff functionalUnit actor ->
  Delegations functionalUnit actor ->
  resource ->
  actor ->
  action ->
  Bool
canGbac rules staff delegations resource actor action =
  any (hasPerms . finalPerms) $ findReverseDelegations (Right actor)
  where
    hasPerms = Set.member action . Map.findWithDefault mempty resource
    finalPerms x = maybe mempty (\f -> Map.findWithDefault mempty x rules) $ Map.lookup x rules
    findReverseDelegations :: Either functionalUnit actor -> Set.Set functionalUnit
    findReverseDelegations start = Set.unions $ unfoldr go (Set.singleton start, mempty)
      where
        go ::
          (Set.Set (Either functionalUnit actor), Set.Set (Either functionalUnit actor)) ->
          Maybe (Set.Set functionalUnit, (Set.Set (Either functionalUnit actor), Set.Set (Either functionalUnit actor)))
        go (toVisit, visited) = go' visited <$> Set.maxView toVisit
        go' visited (current, nextToVisit) =
          let newFunctionalUnits =
                case current of
                  Left y -> Set.singleton y
                  Right y -> maybe mempty Set.singleton $ Map.lookup y staff
              newVisited = Set.insert current visited
              newNextToViisit = maybe mempty (flip Set.difference newVisited) $ Map.lookup current reverseDelegations
           in (newFunctionalUnits, (nextToVisit <> newNextToViisit, newVisited))
    reverseDelegations :: Delegations functionalUnit actor
    reverseDelegations = foldl go mempty $ concatMap (\(s, ps) -> (,s) <$> Set.toList ps) $ Map.toList delegations
      where
        go ::
          Delegations functionalUnit actor ->
          (Either functionalUnit actor, Either functionalUnit actor) ->
          Delegations functionalUnit actor
        go ds (p, s) = Map.alter (Just . Set.insert s . fromMaybe mempty) p ds

-- | OrBAC
-- * https://en.wikipedia.org/wiki/Organisation-based_access_control
data OrganizationMapping subject role' action activity object view = OrganizationMapping
  { subjects :: Map.Map subject role',
    actions :: Map.Map action activity,
    objects :: Map.Map object view
  }
  deriving stock (Show, Eq)

type OrBacRules role' activity view = Map.Map role' (Map.Map activity (Set.Set view))

canOrBac ::
  (Ord role', Ord activity, Ord view, Ord subject, Ord action, Ord object) =>
  OrBacRules role' activity view ->
  OrganizationMapping subject role' action activity object view ->
  subject ->
  action ->
  object ->
  Bool
canOrBac rules mapping subject action object =
  fromMaybe False $
    canAcl rules
      <$> Map.lookup subject mapping.subjects
      <*> Map.lookup action mapping.actions
      <*> Map.lookup object mapping.objects

-- | ReBAC
-- * https://en.wikipedia.org/wiki/Relationship-based_access_control
-- * https://www.osohq.com/academy/relationship-based-access-control-rebac
-- * https://openfga.dev/docs/modeling/getting-started

{-
                - define can_share: owner or editor or owner from parent
                  - Implied (Object "doc:0") "owner" "doc:0"
                  - Implied (Relation "doc:0" "parent") "owner" "doc:0"
                - &&
                - <type>:* public access
                - conditions (ie. time) https://openfga.dev/docs/modeling/conditions
-}
type ReBACRules object relationName = Map.Map relationName (Set.Set (RelationDefinition object relationName))

data RelationDefinition object relationName
  = Direct {user :: Target object relationName, object :: Target object relationName}
  | Implied {user :: Target object relationName, relation :: relationName, object :: Target object relationName}
  deriving stock (Show, Eq, Ord)

data Target object relationName
  = Object {object :: object}
  | Relation {relation :: relationName, object :: object}
  deriving stock (Show, Eq, Ord)

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

-- | Capabilities
-- * For DAC https://en.wikipedia.org/wiki/Discretionary_access_control
type Capabilities token action = Map.Map token (Set.Set action)

canCapabilities :: (Ord token, Ord action) => Capabilities token action -> token -> action -> Bool
canCapabilities capabilities token action =
  Set.member action $
    Map.findWithDefault mempty token capabilities

{-
\* MAC vs DAC
  * https://en.wikipedia.org/wiki/Mandatory_access_control
  * https://en.wikipedia.org/wiki/Discretionary_access_control
    * Biscuit ?
  *
  * Biscuit ?
\* Data-centric security
  * https://en.wikipedia.org/wiki/Data-centric_security

-}
