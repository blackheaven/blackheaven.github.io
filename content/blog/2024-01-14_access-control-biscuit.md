+++
title = "Access Control: Biscuit"
date = 2024-01-14
draft = false
path = "2024-01/access-control-biscuit"

[taxonomies]
categories = ["dev"]
tags = ["access control", "security", "draft concepts"]
+++

Some times ago we have seen [Capabilities](@/blog/2024-01-03_access-control-capabilities.md),
the idea is simple: you have a token (e.g. a string), you provide as a key to
to lookup associated permissions, which is intrinsically stateful.

Another way to look at it is provided by [Biscuit](https://www.biscuitsec.org/),
in which you have a set of facts you match against rules.

Note: rules and facts are expressed in a [Datalog](https://en.wikipedia.org/wiki/Datalog)
dialect, a subset of [Prolog](https://en.wikipedia.org/wiki/Prolog), which is,
in my opinion, the most elegant programming language, and what we should aim at
regarding programming languages/software design.

To check the authorization you have two blocks:

Authority:

```datalog
right("/file1", "read");
right("/file2", "read");
right("/file2", "write");
```

Authorizer:

```datalog
operation("write");
file("/file2");
can_view($file) <- right($file, "read");
allow if file($f), operation($op), right($f, $op);
```

You may also have a revocation list.

The process is done as follows

* The token (encoded and signed by some authority) is decoded and checked (signature and revocation script)
* Both blocks are "concatenated"/resolved (see below)
* guards (e.g. `allow if`) are enforced

Here are the facts after resolution:

```datalog
can_view("/file1");
can_view("/file2");

file("/file2");

operation("write");

right("/file1","read");
right("/file2","read");
right("/file2","write");
```

Biscuit gives us the flexibility to attenuate (i.e. restrict usage) of tokens,
for example, if we want to share the token but only for `read` operation:

```datalog
right("/file1", "read");
right("/file2", "read");
right("/file2", "write");
check if operation("read");
```

then, our _Authorizer_ block, asking for `write` operation.

The interesting part is that the original token is not modified, new facts are
just appended and signatures are chained.
