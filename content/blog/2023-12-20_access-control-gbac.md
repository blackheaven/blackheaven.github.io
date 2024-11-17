+++
title = "Access Control: Graph-based Access control"
date = 2023-12-20
draft = false
path = "2023-12/access-control-gbac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

Last time, we has explored [ABAC](@/blog/2023-12-17_access-control-abac.md) which is
quite elegant in my opinion.

Well, you can forget it, welcome in one of the corporate version of access control.

I have discovered [Graph-based Access control (GBAC)](https://en.wikipedia.org/wiki/Graph-based_access_control)
which splits permissions declaration between types and instances, essentially for
reuse.

In summary for each side:

* An organizational unit (i.e. a department)
* Functional units (manager, layer, etc.)
* Delegation relations

Then, when you have a concrete organizational unit, you instantiate your types
definitions, and you define some agents (employees) belonging to a functional
unit and add extra delegation relations.

If it sounds complicated, it's it because it is.

Let's start with types:

```haskell
type GbacDepartmentRules functionalUnit resource action = Map.Map functionalUnit (Map.Map resource (Set.Set action))

type Staff functionalUnit actor =
  Map.Map actor functionalUnit

type Delegations functionalUnit actor =
  Map.Map (Either functionalUnit actor) (Set.Set (Either functionalUnit actor))
```

* `GbacDepartmentRules` is our quite classic rule table
* `Staff` is our instantiation
* `Delegations` is our graph delegating for a functional unit or an agent to a functional unit to an agent

Then, let's try to type our function:

```haskell
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
```

Breath-in, breath-out, breath-in, breath-out, it's important to stay oxygenized
at this point (the implementation is worse).

Let's break this down:

* You can ignore the two first lines (`forall` and constraints), they are mostly implementation details due to our use of `Map`.
* Then we have the rules `GbacDepartmentRules`
* The actor - functional unit mapping
* Finally the `Delegations` (I have not split types and instances delegations as you can simply merge them)

I'd like to apologize for the following implementation:

```haskell
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
```

Remember to stay oxygenated.

The main idea here is to explore the delegation graph walking backward on the edges.

Each time a delegation comes in, I take the associated permissions, add them to
a list of set of permissions, then I check them until one match, or I there are
no more delegations.

To walk through the edges, I have to first reverse them (`reverseDelegations`)
and, to go through them, take a starting point, keep a list of edges (functional
units or actors), fetch the permissions when I visit one of them, add it to
a set of visited edges (to avoid looping indefinitely) and add next edges
(following reverse vertices).
