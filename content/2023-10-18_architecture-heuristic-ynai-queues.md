+++
title = "Architecture Heuristic YNIA: Queues"
date = 2023-10-18
draft = false
path = "2023-10/architecture-heuristic-ynai-queues"

[taxonomies]
categories = ["dev"]
tags = ["architecture", "design", "heuristics"]
+++

There is a famous acronym I use over and over again when it comes to design decision: [YAGNI](https://en.wikipedia.org/wiki/You_aren%27t_gonna_need_it).

It's a constant reminder to keep things as simple as possible, however we tend
to forget that we should over simplify.

That's one of the reason I tend to introduce early on components which seems to
be more complex than they need to be.

I call it _YNIA_ (You'll Need It Anyway).

Today: queues.

They are many cases when you add accidental complexity by technically coupling
things which are not coupled in terms of business:

- fetching distant (on unmanaged services) some information
- performing long-running operations
- acting on distant services
- running not business-related logic inside or around business logic

There are two basic ways to deal with it: synchronously or through some kind
of local-asynchronous code (e.g. callbacks, threads).

These are not solutions:

- no traceability
- not fault-tolerant
- not restart tolerant
- hard to debug
- hard to evenly scale
- no observability

To give you an idea, last time I have encountered such implementation, we were
regularly loosing jobs each time our pods (on Kubernetes) were restarted (on
failure or on update).
Which was hard to debug as we had to correlate failure of not tracked side-processes
with random restarts.

Adding a queue gives a way to temporally decouple these computations, which
allow to incrementally add:

- a way to monitor each job (current status, history)
- add restart/fault-tolerance (add error detection and mitigation, such as retry)
- handle back-pressure
- add observability (throughput, latency, traceability with [correlation identifiers](https://www.enterpriseintegrationpatterns.com/patterns/messaging/CorrelationIdentifier.html))
