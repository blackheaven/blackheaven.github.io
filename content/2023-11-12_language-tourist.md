+++
title = "Language tourist"
date = 2022-11-12
draft = false
path = "2023-11/language-tourist"

[taxonomies]
categories = ["dev"]
tags = ["dev", "programming languages", "learning", "career"]
+++

When it comes to programming languages, there is a famous quote:

> "A language that doesn't affect the way you think about programming, is not worth knowing."
> 
> -- Alan Perlis, Epigrams on Programming.

I hate this one, a bit of background.

I started my journey in the software development world in 2003 (I was in college,
which let me a lot of time to explore my programming languages, databases, SDLC
approaches, deploying patterns, other software fields).

If I have to collect the programming languages I have learnt these last 20 years
in the order of learning, it would look like:

* PHP
* C++
* HTML
* CSS
* JavaScript
* SQL
* Bash
* Ruby
* C
* LaTeX
* Java
* Eiffel
* Io
* C#
* VB
* XSLT
* Prolog
* Erlang
* Haskell
* CoffeeScript
* Smalltalk
* Python
* Go
* Awk
* Scheme
* Tcl
* Assembly
* Lucid
* Perl
* Elm
* OCaml
* Scala
* F#
* Clojure
* TypeScript
* Idris
* Rust
* Nix
* Lua

I may have forgotten some, or the order might not be correct, but the idea is
to be able to adapt to your environment.

That being said, I distinguish multiple level of mastery:

* Syntactic: being able to read existing code, write some "simple" code (contribute to an existing code base), have basic interaction with build/deployment environment (i.e. building the project)
* Semantic: being able to understand key concept behind the language (beyond their main paradigm: Imperative, Functional, Logic)
* Environmental: get a grasp on the language ecosystem (tools, libraries, frameworks)
* Idiomatic: being able to understand and leverage language idioms and internal structure

There are multiples ways to look at the previous:

* I don't have the same level of mastery in every programming languages
* All programming languages have not the same complexity (i.e. Io and Smalltalk are simple syntactically and semantically, Nix is syntactically simple but semantically complex, C++ is syntactically and semantically complex)
* Many programming languages inherit from the same paradigms and syntax, so I can rely on other idioms to compensate (i.e. whenever I code in F#, I do it like I would in Haskell, well, I try to)

Whenever I learn a new language, I plan/execute my learning as a trip:

* Purpose: is it for my professional growth, for fun (i.e. coding dojos), because I have a contribution to make
* Effort: how hard should I learn? If it's a legacy which should be shutdown in few weeks, reaching the semantic level might be a bit too much
* Collect and sort resources: I try to mix official documentation and "trending" tutorials, selecting perceived important concepts
* In the field: either I'm on production code, so I rely on tests (actual or those I can add) and on my colleagues, or I'm on my own, then I try a pet project for that

I never learn better than in front of the problem (i.e. initially, I have
switched to NixOS to learn about Nix).

My posture is the following: I have to contribute to a code base, most of the
time it consists of fixing business bugs (either business changed, other business
was not understood correctly), which does not take a lot of semantic knowledge.

Sure, from time to time you need environmental/idiomatic knowledge when you
have to build a huge/complete (sub-)system, or debug a nasty behavior (it often
occurs in non-linear logic, i.e. exceptions most of the time).

For the rest, I rely on architectural and design principles (transposed SOLID principles,
coupling, cohesion, etc.)

Note: another important factor, when arriving at a base code, is to grasp
design choices and habits, in my opinions it's one of the biggest challenge as
you cannot really be prepared for that (code review and "culture" can help) and
it varies a lot. For example, I have worked in a team which has created and
maintained three products at the same time, but had really different styles.
