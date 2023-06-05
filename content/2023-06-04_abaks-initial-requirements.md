+++
title = "Abaks: initial requirements"
date = 2023-06-04
draft = false
path = "2023-06/abbaks-initial-requirements"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

A long time ago I "started" a side project called [abaks](http://github.com/blackheaven/abaks), which is a project for personal bank reconciliation statements.

I've been quite busy these lasts months and hopefully my backlog reached the point where I can start working on it.

This project is personal to me as it is one of the thing my mother taught me.

While I could handle it on my own, I have received some requests to have a real-world project exercising `polysemy`.

I cannot come-up with a real-world project on my spare time, of course, but I can build one, explaining the trade-offs along the way.

In particular, I plan to rely on [DDD](https://en.wikipedia.org/wiki/Domain-driven_design), especially [Event Sourcing](https://en.wikipedia.org/wiki/Domain-driven_design#Event_sourcing) as foundation of abaks.
I will rely on my interpretation of [The Clean Architecture](https://blog.cleancoder.com/uncle-bob/2012/08/13/the-clean-architecture.html) (which slightly diverge from the [Hexagonal architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software))).

* Entites will rely on Event Sourcing
* Use Cases will rely on `polysemy`
* Presenters will deal with the back-and-forth of `servant`
* `servant` will be used to expose an API

Note: many shortcuts will be taken, for example, I won't detail all the tests done (except if U get requests on this point), I will also use a file-based event store, without projections.

The basic idea is to log a bunch of entries (expected and actual), when they match, they are "validated", otherwise the entry should be somehow "fixed".

Let's start this series with the basic requirements:

* I should start a _Period_ with a _name_ (i.e. "June 2023"), start and stop dates, an _initial balance_
* I can add/change/comment/delete an _Entry_, which is defined by an _amount_, a _date_, and a category
* An _Entry_ can be expected or not
* At the end, I should be able to validate or flag _in conflict_ an entry

For the moment, I won't:
* Deal with multiple currencies
* Have multiple accounts (i.e. bank account and credit card)
* Handle periodic entries (i.e. monthly phone bills)

Let's begin a new adventure!
