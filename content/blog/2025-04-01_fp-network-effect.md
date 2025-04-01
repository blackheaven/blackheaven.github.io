+++
title = "FP & Network effect"
date = 2025-04-01
draft = false
path = "2025-04/fp-network-effect"

[taxonomies]
categories = ["Software engineering"]
tags = ["haskell", "functional programming"]
+++

Few months ago (my writing, and my publishing queue is a bit long), I've watched
a video of [Dave Farley about Functional Programming adoption failure](https://www.youtube.com/watch?v=gE6nnDsh5Ck).

Note: we could discuss the fact that "FP hasn't taken over the world", given
that all mainstream programming languages have introduced FP-style constructions
and many concepts in FP-related fields in their standard libraries.

Note 2: we could point-out that "taking over the world", hasn't been a goal of
"FP" programming languages, unlike providing a comprehensive and coherent
reasoning framework.

He pointed out few elements:

* Immutability doesn't improve performances of non-parallelizable problems
* Some problems are best expressed in OOP
* FP tends to push concepts too far to be usable on "mainstream project"

I let you go through the video as it covers a lot of interesting historical
details.

I could spend an entire series of logs disagreeing about each element,
or there relevance, but, to keep it short, I don't think the idea of FP
isn't structurally flawed, and some of the current implementations are good
enough.

Instead, I would focus on what I think make FP programming languages not widely
adopted: [network effect](https://en.wikipedia.org/wiki/Network_effect).

> In economics, [..] is the phenomenon by which the value or utility a user
> derives from a good or service depends on the number of users of compatible
> products.

Applied to programming languages: the more users of a programming language,
the more it becomes usable.
You get:

* More libraries
* More Stack Overflow/Discourse/Discord/Reddit/LLMs support
* More tooling/Higher completion quality
* More blogs/books/example/tutorials/videos
* More boot-camps/trainings/school curriculums
* More people to hire

As a quick reminder, not so long ago, Linux and PostGreSQL had a tiny
market share compared to their competitors.
They didn't radically change since then, the context changed, and they became
relevant.

Maybe this time won't come for FP, but that doesn't imply FP is wrong, or
shouldn't be used
