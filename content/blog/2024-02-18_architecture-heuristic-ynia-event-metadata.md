+++
title = "Architecture Heuristic YNIA: Events metadata"
date = 2024-02-18
draft = false
path = "2024-02/architecture-heuristic-ynia-events-metadata"

[taxonomies]
categories = ["Software engineering"]
tags = ["architecture", "design", "heuristics"]
+++

A while ago, I have continued with [_YNIA_](@/blog/2023-11-29_architecture-heuristic-ynia-participants.md),
which is a set of patterns I set up before _really_ need them.

I work a lot (and enjoy working with) [Event Sourced](https://martinfowler.com/eaaDev/EventSourcing.html)
systems.

Events are composed of two parts:

* The business part (which is usually found during [Event Storming](https://www.eventstorming.com/) sessions)
* Metadata (also called envelope), mostly a technical concern

Usually metadata are quite thin:

* UUID
* Creation date
* (Stream ID)
* (Position (index) in the stream)

The great thing about _event sourcing_ is that it captures exactly what happened,
which is an issue if you didn't captured enough, the problem is two-folds:

* You have forgotten an element in the business part: you can try to rebuild an enriched stream
* You have forgotten an element in the metadata part: good luck!

Metadata are a technical concern, until they became a business concern.

That's why I add the following elements:

* Occurring time: when the event happened in real-life (see [bi-temporality](https://martinfowler.com/articles/bitemporal-history.html))
* Actor: who (the person) did the action, it is the person in charge of the delegation (leading to "on behalf of")
* Initiator/on behalf of: who requested the action to be performed leading to the event
* Distributed trace identifiers: to be able to fetch the traces associated
* Origin system: usually en API or a batch/asynchronous process
* Origin system version: usually a deployment version/hash

It is not much, but it covers all the business cases I have faced until now.
