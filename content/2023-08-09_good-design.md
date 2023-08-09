+++
title = "What a good design is"
date = 2023-08-09
draft = false
path = "2023-08/good-design"

[taxonomies]
categories = ["dev"]
tags = ["architecture", "engineering", "design"]
+++

I was pair programming a while ago, when, at some point, when have realized that the initial task was way under-estimated.

So we decided to tackle it right away, taking a new file to lay out the design, I was the driver, I start to frenetically type, until my worker asked "Are you sure?" (speaking of the overall design), to which I answered immediately: "of course not".

It's been more than 20 years that I've been coding, and the most important thing I have learnt is:
I have a good intuition in architectural decisions, but a terrible intuition in code design.
Hopefully my next log will give a concrete example of this.

After all these years I'm still not able to define what a good design is.
I can enumerate many design principles (Loose/Tight coupling, Responsibilities organisation, Interface design, dependencies management) and leverage them to take design decisions.
I can tell that a design is bad because it does not meet a requirement, of make the change meet it difficult, but I can't define what a good design it.

There is a technique called the [Mikado Method](https://mikadomethod.info/), which helps to discover a legacy code base, by trying a refactoring, follow the dependencies of changes, and eventually revert if it takes too long.
The aim being to learn something about the code base, then think of an action plan, and try again.

The same way, I like trying a design, and if, after a short time, if it's too tedious to work with, or make hard to meet requirements, I simply drop it and start over.
While I'm not a big fan of [Fred Brooks' Second-system effect](https://en.wikipedia.org/wiki/Second-system_effect), at least for the complete rewrite part, I find it totally acceptable at micro-level.

Some people argue that if you have to change your architecture it means that it was poorly designed.
But actually, nothing prevent a piece of code to be changed, any actually, if you bet something won't change, it will change.

That's why I write short living code, which does not mean it is badly written code.
It means code properly typed, tested, and just enough coupled to the rest of the code base to be rewritten.
