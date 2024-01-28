+++
title = "Software inventory"
date = 2024-01-28
draft = false
path = "2024-01/software-inventory"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "lean"]
+++

[A while ago](@/2023-11-19_local-global-optimum.md), I had a meetup with [Woody Zuill](https://woodyzuill.com/),
after the [Software Teaming](https://softwareteaming.com/) session, he has
stated that "most companies are bad at _Software Inventory_".

He explained that: _Software Inventory_ is the list of all the code which is not
"in touch" with users.

I thought it was a [Lean](https://en.wikipedia.org/wiki/Lean_software_development)
practice, but the only related topic I have found is _inventory management_
in the original which consist of shrinking down the stock to the minimum
level without impacting clients.
If I transpose it to the software engineering field: you should aim pushing
code as soon as possible, reducing feedbacks loop time (as emphasized in
[Kent Beck's Extreme Programming Explained](https://www.oreilly.com/library/view/extreme-programming-explained/0201616416/)).

Over the years I have set up several ways to have a snapshot of it:

* Adding a protected endpoint which responds the commit hashes of all services
* Include commit hash in the built artifacts

Each time I also built a CLI, to perform a scan and compare the different
environments to the repositories.

It's only a beginning and there are many drawbacks:

* Even following [Trunk Based Development](https://trunkbaseddevelopment.com/), without [CI](https://martinfowler.com/articles/continuousIntegration.html)/[CD](https://martinfowler.com/bliki/ContinuousDelivery.html), you will have some _stock_, which is an issue since you can deploy more (or less) than you intend to
* (Feature) flags hide a part of it (it's tempting to push a lot of code, hiding it behind a flag, and never enabling it)

I had negative experiences with both of them:

* A piece of code which hadn't been deployed to the users for weeks, got deployed after a typo fix, blocking the usage and was impossible to roll back
* A feature was started and protected by a compilation flag for months, in the end, it was not compiling anymore
