+++
title = "Design for collaboration"
date = 2023-04-26
draft = false
path = "2023-04/design-for-collaboration"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "team", "design"]
+++

In a [previous log](@/2023-04-12_cto-retrospective.md) I mentioned that I some point I totally "give up" some of my architecture choices.

Let me start by stating that I'm comfortable writing in Haskell in Type-Driven Design and [TDD](https://en.wikipedia.org/wiki/Test-driven_development), following [DDD](https://en.wikipedia.org/wiki/Domain-driven_design) principles, in an [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html)/[CQRS](https://martinfowler.com/bliki/CQRS.html) context, in a [reactive architecture](https://www.reactivemanifesto.org/)i, organized according to [the Clean Architecture](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html).
I could add that I prefer NoSQL databases over RDBMS (even though I love PostGreSQL), I also heavily use Property-Based testing for my domain code, and few tests for my APIs.

Let's imagine for a minute that you start on a project like that, without any prior knowledge on half of the concepts mentioned previously.
How likely would you be able to make great contributions quickly?
I would say low.

Whenever you architect a system, you should take in account the builders of this system.
Otherwise, you are paving the way for failure.

Sure, your system may need it because the constraints requires it, but if you build it, either it won't be maintained, or it won't be used (correctly), but it will create unnecessary pain.
The good thing is, from my experience, you'll end up with this solution (or a solution close to it), eventually.
It's easier to lead a movement than starting it (i.e. it's easier to refine your coworkers' vision, than imposing yours).

The thing to look after is _reversible decision_, each time a decision is taken, you should imagine an "escape plan".
If it's not too costly, this decision is fine, otherwise (often it involves data-loss), the decision should be debated.
