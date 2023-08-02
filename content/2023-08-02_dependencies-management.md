+++
title = "Dependencies management"
date = 2023-08-02
draft = false
path = "2023-08/dependencies-management"

[taxonomies]
categories = ["dev"]
tags = ["architecture", "engineering"]
+++

A while ago I was working with a product-owner which was really, really keen on integrating SaaS solutions in our product as a way to bring value to end-users.

I was always reluctant to integrate new services as it impacts [SLOs](https://en.wikipedia.org/wiki/Service-level_objective),
which may be make our product not usable. Moreover, integrating a SaaS solution is impactful, you have to study APIs (if any), discover limitations as you go, etc.

First and foremost, for each proposition I had to get to the root of business need, then I was able to apply [Wardley mapping](https://en.wikipedia.org/wiki/Wardley_map).

Which gives me a hint of the value of the solution.

Of course, another factor was the complexity of the feature (regarding current and future needs).

Over time, I developed the following heuristic:
* Simple feature, no data stored as source of truth: redo
* Average to high complexity, no data stored as source of truth: use a library
* Any complexity with data as source of truth: use a SaaS/PaaS product

Aside of the consideration of have acceptable SLOs, ensuring to prevent data-losses is a big concern to me.
While non-usability is temporary, loosing data is forever damageable.
Ensuring yourself to have appropriate counter-measures (tested backups, redundancy, monitoring, etc.)
is time-consuming and require advanced skilled to be done right (moreover, I tend to sleep better whenever I know someone competent is taking care of that).

In the end, I think my strategy worker well, as over three years, our product usability was only affected once, while our business need was met quickly.
