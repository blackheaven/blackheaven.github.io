+++
title = "Access Control: Organisation-based Access control"
date = 2023-12-24
draft = false
path = "2023-12/access-control-orbac"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

Last time, we have seen [GBAC](@/blog/2023-12-20_access-control-gbac.md), probably the
most access control scheme, let's go a little simpler.

[Organisation-based Access control](https://en.wikipedia.org/wiki/Organisation-based_access_control) is
a scheme based on the distinction between security policy and implementation.

It's a mix between [ACL](@/blog/2023-12-06_access-control-acl.md) and [RBAC](@/blog/2023-12-13_access-control-rbac.md).

On one hand, there is the security policy defined with `Role`, `Activity`, and `View` (target).
On another hand, the implementation based on `Subject`, `Action`, and `Object`.
Finally, you have a contextualized mapping from `Role` to `Subject`, from `Activity` to `Action`, and from `View` to `Object`.

Let's formally define it:

```haskell
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
```
