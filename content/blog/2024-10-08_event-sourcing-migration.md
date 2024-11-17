+++
title = "Event sourcing migrations retrospective"
date = 2024-10-08
draft = false
path = "2024-10/event-sourcing-migration"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design", "retrospective", "architecture"]
+++

Few weeks ago, I was talking to one of my co-workers about [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html).

At some point, he was inquiring about retrospectives on migrating old systems
to event sourcing, benefits obtained, pitfalls to avoid, and so on and so forth.

I have looked around a bit, ask some friends, nothing relevant came up.

I'll try to summarize my 6+ years of experiences in this log.

## Experiences

### Distributed mail server

*Business context:* A FOSS company, providing a G-suit alternative.

*Team context:* 6 seasoned SWE, already accustomed to software craftmanship,
DDD, and event sourcing.

*Technical context:* it was a 10 years old Java project, mostly relying on
Cassandra, when I have joined, they were in the middle of a complete
modernization.

*Challenges:* Solve concurrency issues, especially in the mailboxes area, most
of the bugs are in this area (race conditions occurs when a new e-mail were
incoming, while the user was working on them).

*Event sourcing scope:* the mailboxes, very few events were involved.

*Pitfalls:* the event sourcing implementation was quite complex and quite limited,
which means many of the operations were stateful, leading to a global state
permanently incoherent (e.g. two instances connected to the same Cassandra
cluster will has two distinct states). Add new events involved a lot of
boilerplate.

*Benefits:* Mailboxes concurrency issues were fixed, the implementation was
comprehensive enough to have a lot of tools (debugging, rebuilding projections).

### Shared dashboard

*Business context:* A pre-seed startup looking for its product-market-fit.

*Team context:* 4-people team, mostly juniors, discovering DDD.

*Technical context:* A brown-field Haskell project I had started few months
ago, using AWS DynamoDB as primary data store.

*Challenges:* The project was stressed by numerous changes. We also had to
figure out what the user was doing, especially when discovering new corner-cases
(also called business opportunities).

*Event sourcing scope:* the whole organization, each "group" was in the
aggregate, alongside with users management, dashboards, etc. everything.

*Pitfalls:* Team adoption was a huge pain point, most of them considered it
as a "waste", also, intents were missing (Commands should have been logged),
migration was done in big-bang mode (7-day effort), which created some tensions.
Keeping original projections in the discovery phase added burden without any
benefits.

*Benefits:* Each new workflow or concept took only a few hours to implement
in the backend, events and commands where exposed in the API endpoints, new
events where pushed asynchronously.

### Distributed business process

*Business context:* A mature B2B company customer-focused with a very
comprehensive (yet adapting) business processes.

*Team context:* 6-people team, mostly seniors, with some DDD/Event sourcing
knowledge.

*Technical context:* 3 years project, microservice based, each of them had a
specific data/events store (EventStore, plain files, SQLite) and design discipline.

*Challenges:* The business process was really complex (40 steps, 60 events),
constantly changing.

*Event sourcing scope:* Only the regular, customer-facing process.

*Pitfalls:* Each microservice had their own events, trying to integrate
each-other events, loosing information along the way, events were also rigid,
no event could be added easily, or modified, or the stream rewrote. Most of the
computation were done around a single stream, projections being computed at
startup (45 minutes). While only few things were outside the system, but it
was the missing piece to have a stable stream.
Every microservice were involved in new features.
A big events re-design was planed, but never implemented.

*Benefits:* Each microservice had their own events, giving a lot of flexibility,
limited changes (such as new projections) were easy.

## Lessons learned

* Events shouldn't be set in stone (they aim to evolved, be updated, deprecated, rewritten)
* Big bang rewrites are bad, but incomplete migrations are harmful
* Big aggregates are okay, in the literature, aggregates should have few events, in practice, big aggregates are better than a lot of smaller ones involving synchronization
* Focus on developer experience, focus on tooling and easiness to add events
* Don't focus on projections, they could be added later, if they are really popular and critical
