+++
title = "Retrospective of my time at Autogriff"
date = 2024-09-03
draft = false
path = "2024-09/autogriff-retrospective"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "retrospective", "feedbacks", "team"]
+++

As you might have noticed, I've [switch position](https://livtours.com/) few weeks ago.

After talking with my former CTO, it seems to be a great idea to do a
retrospective on my experience at [Autogriff](https://www.autogriff.com/).

### Context

Autogriff is a B2B company specialized in cars fleet management.

If I have to describe the business workflow: a company signs a contract with
Autogriff, and whenever something happen to one of their car (accident, driver
change, etc.), Autogriff handles the estimation, finding and dealing with repair shops.

It was founded in 2017, and currently has ~15 employees.

The IT department was created 4 years ago, it ran for a long-time with juniors
developers (internships or retrainings), senior developers onboarded 2 years ago.

The business side is quite mature, but it is still flexible, so they adapt their
workflows to their clients.

Note: at this point, it's probably the most customer-oriented company I have worked for

Which means that it was challenging for the IT.

### Technology overview

On the technology side, Autogriff suffers from two things:

* Few years of "architecture" made in autonomy by juniors: C#, Java, Python, CRUD and Client-First architecture
* The arrival of the senior developers: Haskell, Elm, Rust, Nix, DDD, Event sourcing, CQRS, microservices, [Capabilities-based](@/blog/2024-01-03_access-control-capabilities.md), HATEOAS API

We had few big blocks:

* A Java tool (legacy) holding invoices, clients, and some metadata loosely coupled with interventions
* A Python (legacy, Event-sourced) / Elm (legacy) tool for interaction with interventions
* A Haskell (Event-sourced) / Elm tool for the interventions dashboard and business intelligence (my work)
* An Elm PWA for interacting with drivers

### Learnings and confirmations

There are few things I have already encountered, which still hold:

* CRUD-based UI spreads: when you have that kind of UI, coming with a different architecture for the backend is impossible
* Architecture is not improvised: it's tempting for non-technical decisions maker to see IT as a building, allocating resources on a bigger team of juniors, rather than a small one with juniors, but it doesn't work if you don't have enough software engineering background
* Technical debt explode when not repayed: at some point, technical debt prevents to deliver some features (even approximation), tu give an example, when you have and data you have to compute, from two sources, one being event-sourced, the other not, you end up with invalid results
* Lack of fast feedbacks is a deadly killer: we had Nix-based deployments ([deploy-rs](https://github.com/serokell/deploy-rs)), which relies on systemd, meaning, you can have only one instance running at the same time (forget zero downtime deployments), which is an issue when it takes 45 minutes to restart the backend (mostly the time to replay events to have fresh projections, I have added a cache to reduce to few seconds). Consequently, you have two deployment windows (after the working day, or at lunch for emergency fixes), it means that, even for one line fix, you have to wait for a full-day to be used by users (causing context switches and creating hard conversations with business)
* Lack of automations make deployments a big deal: I have introduced continuous integration when I arrived, but continuous deployments were not a thing, not only we had the previously mentioned restart issue, but I was done manually, mostly by 2 people (I was one of them), owning the deployment tool, who should be aware of the manual configuration to be performed, while, when I have set up an ArgoCD based CD, everything was done automatically, and there was no difference between a commit and a deployment.
* Mono-team microservices/multi-repository don't work: we had 4 repositories for 5 heavily coupled parts, which means, we had to commit everywhere often
* Opinionated debated/conservative decisions is a blocker: a lot of discussion happen were running over days to take trivial [two-way doors decisions](https://thoughtbot.com/blog/one-way-vs-two-way-door-decisions), for example, I have silently put the startup cache in two hours (and I have fixed it 3 times), while we have discussed it for two months (not full-time), even when it was actually used for weeks
* Lack of automations make everything hard: when you do everything by hand, everything is unique, which makes things harder to automate, automating imply standardization, which implies giving up some details
* Code reviews culture is hard to bootstrap: when reviewing is not part of the culture, it's hard to cope with it (even small PRs can take days to be reviewed, approvals are hard to get)
* Haskell catches a lot of bug: even with fewer tests than I would have on Java project, sufficiently typed code catches most of the bugs, I had fewer regressions thanks to it
* Nix provides a good foundation for infrastructure: everything works with Nix (from development environment, to deployment), it took me roughly 30 minutes to have an environment and a first binary, and I can't remind a situation where someone said "but it works on my computer"
* DDD is the corner stone of working software: we have spent countless hours speakings with the business, adjusting the vocabulary, fixing issues before they happen (we even organized an EventStorming which help us to fix many problems, set our priorities, spot many bugs, and make the business aware of their inconsistencies)

Some learnings I wish I didn't make:

* HATEOAS-only sucks: for reference HATEOAS is a way to have endpoints described in response payload, which is great in theory for discovery and decoupling clients, but in practise, the lack of tooling (OpenAPI does not support it, and we had to make a lot of work in our adapters in Elm to make it work) is painful and giving this kind of API to customers was an issue
* Capabilities-based alone is not enough: while decoupling access and user being a good idea, at some point you have to answer to two questions: who accessed to a resource and what are the accesses of a user
* Event-sourcing can be rigid: when you prevent yourself to rework your events, it becomes more painful than a plain old relationnal database
* Elm is too simplistic to go beyond one page: while we had a relatively simple set of apps (3 apps, with 5-10 pages), Elm was painful to use, the standard library is too small to be useful and the [nested TEA pattern](https://sporto.github.io/elm-patterns/architecture/nested-tea.html), which is the standard way to handle "complex" apps (mostly a consequence of a lack of "advanced" features), make sub-components not composable

Finally, my observations and learnings I'm glad I've made:

* Even business-oriented team can be highly technical: I have rarely work in business-oriented team (on non-technical products), applying DDD that, so I didn't expect them to also being highly technical
* Capabilities does the trick: while capabilities introduce some complexity, I have seen that it works well, at least for many use cases, for few efforts
* Developers from retrainings brings a lot of (human) value: it took me maybe 8-10 years (since my first encouter with programming at 9) to have a grasp on software engineering, so I have a lot of suspicions when I have to work with people coming from a 6-weeks bootcamp, but it occurs I have worked with a junior having great communication style, asking a lot of questions, dealing with other business units, bringing a lot of value
* Sometime bringing values consist in helping those who bring more value: I've spent a lot of time pairing with juniors developers, and it seems I'm not that bad at coaching, and it seems supporting them was a better use of my time because, while they have a good business vision (they were in the company long before me), they lack a technical vision I can bring
* Pace correctly a pair programming session: spending a lot of time in pair programming gave me the chance to experiment a lot, especially finding and adjusting just the right pace in pair programming, being supporting of my pair
* Being a better subordinate: one thing I have learned about me when I was a CTO, is what I expect from my team, getting back in an IC role, I gain a better understanding of my CTO needs and expectations, acting according to them; moreover, I was able to appreciate my CTO competencies, sure, he has a different style than mine, but he is really great at a lot of things, I have learned so much.

### Why moving on?

I have spent a bit more than a year at Autogriff, which is too short.

I'll go in depth in a future log, but the short answer is applying Jeff Bezos' Regret minimization Framework.

Over the past years, I have declined 3 offers I deeply regret today, and this time I have a rare match between an offer and my objectives.

I have thanks my colleagues for all these months of collaboration, learning and help, I have spent a wonderful time with all of them.

PS: Yes, I miss my colleagues, I miss the context, but it's a new adventures ahead!
