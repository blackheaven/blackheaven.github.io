+++
title = "Types and Tests"
date = 2024-02-21
draft = false
path = "2024-02/types-tests"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "design", "engineering", "type-driven design", "test-driven development"]
+++

There is a bit of hype around [Test-driven development (TDD)](https://martinfowler.com/bliki/TestDrivenDevelopment.html)
these years (I am clearly biased since that's one of my [meetup group](https://swcraftlyon.github.io/) focus).

The idea behind this is simple to state:

1. Red phase: add a broken test (it should build, yet, you have to see the assertion fail)
2. Green phase: make the simplest possible (even if it does not intend to be the "final implementation")
3. Refactor phase: refactor to simplify or make the code clearer (tests should still pass, changes should not introduce new behaviors)
4. Go to (1)

However, it is really hard to master:

- Pick the next test (not too close to what we already have, not too far so the step is too big)
- Come up with the simplest implementation
- When and how much to refactor
- Design [qualitative](https://kentbeck.github.io/TestDesiderata/) tests

To be really honest, I'm not sure I will ever master it eventually.

On another hand, there is _Type-driven design (TyDD)_, or what I call _Structure-driven design (SDD)_.
There are far fewer materials about it, the closest I know is: [Domain Modeling Made Functional](https://fsharpforfunandprofit.com/ddd/),
which is a great book but starts with data-types, while I prefer starting with functions.

The process is more involved:

1. Choose a use case
2. Pick a function name and types (inputs and output)
3. Compose functions until:
  a. The function is complete (i.e. the compiler tell you so), then go to (1)
  b. You lack of functions to complete it, then go to (2)
  c. The completion does not make sense, then refine types (2)

It is really a top-down approach with rely mostly on compiler guidance (it can
be done in dynamically-typed programming languages, but it requires strong typing).

I mostly use this approach as it gives me a fast, precise, and systematic feedback.

Usually materials oppose types and tests, but they have two complementary purposes.

- Types
  - Document composability
  - Express system's semantic (meaning)
  - Stabilize (notify early) system's structure
- Tests
  - Document usage
  - Express system's expectations
  - Stabilize (notify early) system's behavior

Both drive the system's design during inception phase, both ensure correctness,
both work in conjunction.
You cannot have good tests without good types, or good types without easily
writable tests.

In my practice however, I tend to push types as far I can until I cannot enforce
behaviors at type-level, then I rely on tests (however, given small enough
functions, it is not a big part).
