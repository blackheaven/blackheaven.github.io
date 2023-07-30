+++
title = "A case against Haskell immersion"
date = 2023-07-30
draft = false
path = "2023-07/haskell-immersion"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "engineering", "career"]
+++

I have just reached the 3-years-mark using Haskell in production, after 3 years as freelance Haskell teacher.

First and foremost, I'd like to emphasize that I have willingly chosen to work in Haskell,
and I think, as a developer, it was one of the best move I could do.

Over these years, I was able to ship a lot of business value to the users,
I have never hesitated doing huge refactoring of architectural changes just because I would be afraid of (silently) breaking something,
I had the fastest feedback on my design I never had (thanks to GHC, which can be stubborn sometime),
I was able to make major changes in my applications' semantic in no time.

Of course, Haskell is not the only factor ([DDD](https://en.wikipedia.org/wiki/Domain-driven_design), [EventSourcing](https://martinfowler.com/eaaDev/EventSourcing.html), [CQRS](https://martinfowler.com/bliki/CQRS.html), [Test-Driven Development](https://martinfowler.com/bliki/TestDrivenDevelopment.html) played a role), but it clearly was a pillar.

Aside of my day-to-day job, I'm one of the organizers of the [Software Crafters Lyon](https://www.meetup.com/fr-FR/Software-Craftsmanship-Lyon/),
which focus on practising and reflecting around [Software Crafting](https://manifesto.softwarecraftsmanship.org/).

Along the way, I noticed that the more I was digging into Haskell ecosystem,
the problem my concerns and my group members concerned diverged.

For example, while scaling up projects in Haskell in generally not a big deal (thanks to a compiler-backed composition),
it seems to be a concern for others.
Conversely, my main struggle was to find Haskell-grade libraries
(I never had a situation where I didn't find a library for what I had to do, but I much more picky when it comes to choose a library that I was when I was working with mainstream languages).
I also tend to use alternate technologies (Elm vs Angular/React, elm-ui vs Pug/SASS/SCSS, Nix/NixOS vs Docker/Kubernetes).

Which made my discussions not always productive with the other members of my group.
Also, I could say that it can be an issue during hiring as many recruiters are focused on a specific set of technologies,
disregarding general knowledge on software
(and my vision and my approach of software engineering changed a lot, thanks to my intensive Haskell practise).

Anyway, I'm still glad to work in Haskell, and I'm glad to be part of the Software Crafters Lyon,
as I learn every day, infusing knowledge from each world.
