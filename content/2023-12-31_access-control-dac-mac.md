+++
title = "Access Control: Mandatory vs Discretionary"
date = 2023-12-31
draft = false
path = "2023-12/access-control-dac-mac"

[taxonomies]
categories = ["dev"]
tags = ["access control", "security"]
+++

We have seen many schemes since [the first log of the series](@/2023-12-06_access-control-acl.md).

Most of the time, our draft implementations were focused on the user/actor.

That's called [Mandatory access control (MAC)](https://en.wikipedia.org/wiki/Mandatory_access_control),
usually enforced by the system (and centralized), it consists of a set of policies
specifying users/actors clearance (their permissions).

It is opposed to [Discretionary access control (DAC)](https://en.wikipedia.org/wiki/Discretionary_access_control),
usually decentralized, policies are expressed around object/resource.

It's not clear, but usually actions depends on ownership (i.e. file owner can
read/write/execute a given file, the other members of the owner's group can
only read/execute, and every other can only read it).

Note: in the next log, we'll have a look at another way (i.e. not ownership)
to express DAC.

Globally MAC are harder to implement, clearer and more secure, but more rigid.
