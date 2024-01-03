+++
title = "Access Control: Capabilities"
date = 2024-01-03
draft = false
path = "2024-01/access-control-capabilities"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "access control", "security", "draft concepts"]
+++

In the [previous log](@/2023-12-31_access-control-dac-mac.md), we have introduced
[DAC](https://en.wikipedia.org/wiki/Discretionary_access_control), I have stated
that ownership is not the only way to do it.

Let's introduce [Capabilities](https://en.wikipedia.org/wiki/Capability-based_security).

The basic idea is similar to physical key: you have a token (usually identified by a random string) with
associated permissions, then, when you have to perform an action, you check
against them.

Let's sketch this out:

```haskell
type Capabilities token action = Map.Map token (Set.Set action)

canCapabilities :: (Ord token, Ord action) => Capabilities token action -> token -> action -> Bool
canCapabilities capabilities token action =
  Set.member action $
    Map.findWithDefault mempty token capabilities
```

It's the simplest scheme we have seen so far.

A common practice is to limit token to few resources and permissions, which
gives really fine-grain access control.

Moreover, you can easily extend (add new permissions), attenuate (remove permissions),
or revoke them specifically.
The another option, if your system allows it, is to emit new tokens with attenuated
permissions in order to share them.

There are two main drawbacks:

* You have to set up some kind of keychain (a bit like Bitcoin's wallets which contains all keys to access to each transactions' outputs, Bitcoin does not work like a purse where all the coins are put, instead you get access to a set of transactions' outputs and you "give" some outputs accesses to create new transactions)
* Tracking accesses (i.e. linking accesses to specific actors) is difficult
