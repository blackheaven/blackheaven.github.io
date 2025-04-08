+++
title = "Bug fixing guerilla"
date = 2025-04-08
draft = false
path = "2025-04/bug-fixing-guerilla"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "product", "design"]
+++

During the last end of year period, the two last weeks, have collected
"small" issues and push 12 pull-requests.

These were either, long-standing issues labelled as "minor", or small annoying
elements in the UX.

Actually, I've got tremendous of positive feedback on them.

Why if so?
[Aggregation of marginal gains](https://jamesclear.com/marginal-gains).

Most of my fixes were related to the most used workflows.

To give a practical example: we have a support team dedicated to help customers
regarding there order.
They receive a ticket, with identifiers of 5 entities on 3 different tools.
They open each tool, check the status of each entity, copy-pasting identifiers
all around, and replying to the customer.
It takes them between 6 and 10 minutes to complete each ticket.

A Perfect solution would require to create a specific UI, summing up each
entity status, but it would take at least few days, instead, adding links to
each status page took me less than an hour and saves 1 to 2 minutes per ticket.
It reduced friction.

Sure, working on big project is important, building a comprehensive tests-suite
is fundamental, but these are future-oriented.

Opening the product you're building, apply extreme programming (XP)'s zero
defects mentally is present-oriented, for the users you have.
