+++
title = "Naming has a social convention"
date = 2024-02-04
draft = false
path = "2024-02/naming-social-convention"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design"]
+++

Few months ago, I have joined [GDCR](@/blog/2023-11-05_gdcr-summary.md), ending the
day with a [software teaming](https://softwareteaming.com/) in F#, with _one
letter identifiers_ as a constraint.

The goal was to work on the [Mars Rover Kata](https://www.codurance.com/katas/mars-rover).

The resulting code was cryptic and the main entry point was:

```fsharp
let p i c o =
  ...
```

* `p` is the predicate making the position evolve
* `i` is the initial/current position
* `c` is the command (move, rotate left, rotate right)
* `o` is the list of obstacle

It was not great but all the participants agreed on it, and we all knew what
each identifier was representing.

Even if the example is extreme, when we try to implement the [Ubiquitous Language](https://martinfowler.com/bliki/UbiquitousLanguage.html)
we need some flexibility when choosing the names, so everyone has a common
understanding.

The main point is, naming decision are always taken inside a group, not on your own.
