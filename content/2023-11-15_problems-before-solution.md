+++
title = "Problems before solution"
date = 2022-11-15
draft = false
path = "2023-11/problems-before-solution"

[taxonomies]
categories = ["Software engineering"]
tags = ["architecture", "design"]
+++

A while ago, I have the chance to kick-start a new project, our product
owner just finished presenting his that one of my colleague started to decide
which tools we had to use.

It was a surprise to me since he proposed to pick PostGreSQL and [Hexagonal architecture](https://en.wikipedia.org/wiki/Hexagonal_architecture_(software)).

The whole point of `Hexagonal architecture` is to be able to delay decisions
(especially technical decisions), which does not really fit with my idea of
picking a relational database management system.

Anyway, we have started to layout folders and files:
 * `API`: Servant endpoint
 * `Database`: queries and encoders/decoders
 * `Domain`: only types, not business logic
 * `Repository`: mostly queries coordination
 * `Utils`: various technical code

Most of the business logic was spread between `Database` and `Repository`, which
leads to an impressive mix of data and temporal coupling.

Not only that, but tests were only done at API-level.
Tests were required a dedicated PostGreSQL instance, they were running in 30
seconds (even with transactions disabled, which differed from production runtime).

To make the situation worse, the "product" was in ideation mode, meaning we
were in a quick feedback loop.

At each concept introduction, a large part of queries and tests were moved.

The fundamental issue was the technical focus from the beginning.
Instead of focusing on business concerns, we jumped on a complete RDMS,
which is a double issue.
PostGreSQL is developer-friendly, meaning it can do a lot of things, you are
able to (too) easily fulfill many business use-cases.
Consequently, the incentive to extract and isolate business code is low,
especially if you are not used to.

> “Nothing is more dangerous than an idea when it is the only one you have.”
> 
> ― Emile Chartier Alain
