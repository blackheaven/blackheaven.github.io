+++
title = "Team owned architecture"
date = 2023-10-01
draft = false
path = "2023-10/team-owned-architecture"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "team", "leadership"]
+++

In early 2013, I was in my penultimate year of my software engineering degree.

At the time, we were split in groups of 6 people to work on various project
(ranging from technical to more management-oriented projects).

At some point, we decided to take leaderships according to our pre-existing
skills (which is a great tactic if we want to have a high quality delivery
for the best efficiency, but a poor learning/long-term strategy).

We had a big home automation project (12 weeks), at the time, Google was
just starting to communicate their plans about it, and the aim of the project
was to build a home automation center.

We decided to embed a Java server with a WebUI on a Raspberry Pi (which was launch a year before).
I took the lead on it, laying out the architecture.

It was mostly interface-based:

* An interface for describing devices (and there probes, such as temperature, and actions, such as switching the light)
* An interface for describing data sources (such as local weather website)
* An interface for WebUI components building

On top of that, we had a rule system which was connecting everything.

It was a giant failure in terms of management:

* We worked too hard on it (I worked until the presentation day, sleeping 2 hours)
* I barely explained myself about the design (I actually only support on how to work with it)
* I left little room for debate (my usual reply at the time was trust me: "it will work")

I was actually feeling bad about prior to the presentation.

At some point, one of the teachers asked:

> So, you have a monolithic Java application, does it mean that you are
> forced to recompile each time you want to add a new device type?

I went blank.

One of my teammates jumped on it:

> No we don't, as Java allows you to dynamically load code

Another one continued with:

> We have interfaces for devices and WebUI, so we don't even need to change existing
> code to be extensible.

I was impressed, even with a lack of communication, they were able to strive in the architecture,
and to own them.

Aside of that, the results were pretty good:

* Each of my group mates were able to work independently (each one of them being able to focus on their favorite part, writing devices driver, creating WebUI components, rules)
* The WebUI was clear and swift
* The automation was working like a charm
* We have got the best grade of the class

It took me a while to learn from this experience, but since then, I have learnt to:

* Protect my team from overload (as much as I could)
* Discuss and argue about architecture (instead of forcing it)
* Take small steps when setting up an architecture
* Build a sustainable product development environment

Over the years, I think of it more and more, should I judged it:

* From a process point of view
* From an outcome point of view

If I transpose it to my day-to-day work:

* Should I create technical debt (of sacrifice my team/personal well-being) to meet a business deadline? (being effectively important or not)
* Should I favor a middle/long-term plan, and risking the loss of unplanned opportunities?
