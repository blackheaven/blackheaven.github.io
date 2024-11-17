+++
title = "Retrospective of my first CTO experience"
date = 2023-04-12
draft = false
path = "2023-04/cto-retrospective"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "retrospective", "feedbacks", "team"]
+++

As I'll soon change company, I think it's the right time to do a retrospective of these years as CTO, for the first time.
(Note that, legally speaking I've been "promoted" CTO after more than one year, but, in practice, I acted as CTO from day 1).

My company was, at the time, an early-stage startup of 4 people (including 3 co-founders, excluding me), which did a first product, and was looking for a new concept to leverage feedbacks.

I started in early September in a company which aimed to provide a platform for product teams.
This platform was interacting with various tools (Github, Gitlab, JIRA, Trello, etc.).
We started with a team of two individuals (a frontend engineer I hired, and me).
As I knew I won't have much time, I preferred each one of us stay in their comfort zone (I will write a log on it), which meant Angular/TypeScript for the frontend, Haskell for the backend, AWS/Terraform for the infrastructure.
I started very optimistically at the time, putting everything in place for a long development (e.g. [ADR](https://adr.github.io/), a microservices/[EDA](https://en.wikipedia.org/wiki/Event-driven_architecture) based architecture, a solid CI/CD).
It was a quite exiting time, we were pair-programming a lot, having a short feedbacks loop in the company.

The main issue we had at the time, was a problem of speed: we were only 2 developers, and there were 4 individuals gathering feedbacks and pushing features.
Along the way we tried to rely on freelancers (we hired 6 of them over 6 months), but it was time-consuming and the on-boarding was tedious.
For instance, I'm based in France, and I some point I had to onboard someone in Vietnam and someone in Brazil.
The ADRs, weren't used/useful as they involved a lot of knowledge in Software Engineering.
Anyway, as mentioned in [Fred Brooks](https://en.wikipedia.org/wiki/Fred_Brooks) [The Mythical Man-Month](https://en.wikipedia.org/wiki/The_Mythical_Man-Month), adding people does not save time in the beginning.

At some point we decided, with the co-founders, to grow the permanent team, we hired 3 more developers (I plan to write a log on hiring too).
It was a big change, previously, hiring a freelancers was quite simple:
- Look for freelancers on upwork
- Check their profile
- Look at their Github account
- Chat with them
- Hire them for a week, and renew if it works

To a more structured hiring, process.

Luckily we got experienced newcomers (including one which was much more experienced that I was).
It was interesting to discuss all the choices I made the previous months, sure, I was expecting that many of them would be "negatively" challenged (ie. "why have you done that?"), but some of them were "positively" challenged (ie. "you should have gone further").
That's when we migrated to [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html) and [CQRS](https://martinfowler.com/bliki/CQRS.html), and it was painful, but our reactivity to changes skyrocketed.
It was also the time when I had to act as a manager, while, until then, with enough pair-programming, the team was stable.

After some months, we realized that we didn't get enough traction, we actually stopped this product (actually, we created a new toy project from it, shrinking down features), loose a part of the staff (including a co-founder, and the first frontend developer).
We hired new developers and started a new collaborative tool.
The difference was that, this time, I wasn't alone to work on the backend, the other developer was really fast but had a different background from mine.
The new architecture was drastically simpler (PostGreSQL instead of DynamoDB/ElasticSearch, a monolith instead of a micro-services based architecture, no event).
Everything went well, the pace was high, we had many feedbacks, too many feedbacks, our tool was too generic, we had to target a specific group of users, leading to a major pivot of the tool.
It was actually painful, changing a lot of concepts, database tables, functions, and queries was costly.
Eventually, after that we started to have more relevant feedbacks and our first users.

This is when I decided to look for new opportunities.

If I have to take few steps back, while it was a great experience, I wish I have done some things differently:
- While I tend to favor consensus, I have imposed few decisions in order to have a "manageable" product, but it led [a bus factor of 1](https://en.wikipedia.org/wiki/Bus_factor), so I had to rework some parts during my notice period
- I wish I didn't use Haskell for the new product, because it makes hiring difficult in early-stage startups
- I should have "silo-ed" more product team and business team, actually business people were kind of geeks, and on another hand product team members were not used to speak business language, leading business folks to push for technical solution (instead of business problems) and developers raising technical issues (instead of challenging the business)
- I should have insisted more on conceptualizing things instead of relying purely on UX to build the product
- I wish I would have been more articulated when I was advocating for tests, code quality or fast and regular delivery and feedbacks

However I'm proud of many things:
- Being able to build a great team is quite a big achievement to me
- While I wish some (architectural) decisions to be taken differently, it was great that once my coworkers get the issues they were solving, my colleagues adopted them naturally
- I had a role of "enabler", taking-out the big rocks on the road, enabling everyone to have a smooth working environment
- I find a way to many various workloads (development, infrastructure, meetings, reports, administrative tasks, hirings, etc.) quite smoothly
