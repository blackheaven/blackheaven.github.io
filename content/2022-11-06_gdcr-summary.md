+++
title = "GDCR 2022: My feedback"
date = 2022-11-06
draft = false
path = "2022-11/gdcr-summary"

[taxonomies]
categories = ["Code practice"]
tags = ["gdcr", "code kata", "coding dojo", "code retreat"]
+++

As every year, the [Global day of code retreat](https://www.coderetreat.org/events/) is a good excuse to work and rework the same kata a full day.

This year I have attended to the one organized by the [Software Crafters Lyon](https://twitter.com/swcraftlyon)
(I'm a member of the organizing team, but I didn't organize the Code Retreat this time).

_Note:_ (For a global/official summary, have a look [here](https://swcraftlyon.github.io/posts/2022-11-05-code-retreat/) (in French)

## Subject

It was a custom subject which initially aim to support [Hexagonal architecture](https://web.archive.org/web/20180822100852/alistair.cockburn.us/Hexagonal+architecture) during team coaching.

* Theme: Expose a Web Service that displays SpaceX launches CO2 cost in a given period of time
* Source API endpoint: [https://api.spacexdata.com/v5/launches](https://api.spacexdata.com/v5/launches)
* Rules:
  * Each rocket built emit 2 536 485 kg of CO2
  * Each rocket launch emit 336 552 kg of CO2
  * The cost of the rockt construction is included in the launch(es) for which the request was made
  * The cost should be expressed as:
    * Number of Kg of CO2 emitted
    * Equivalent in Kgs of beef (1 Kg of beef takes 13.1 kg of CO2 to produce)
    * Equivalent in flights Paris to New-York (1.178 tons)

## My iterations

1.
  * Language: Haskell
  * Paired with a seasoned crafter
  * It was an exploratory session
  * I was with another longtime crafter I used to pair with
  * We focused on computing the total sum of CO2 and equivalents of multiple launches
2.
  * Language: Java
  * Paired with a newcomer
  * Additional constraints: Immutable and/or Blind navigator
  * We barely achieved CO2 sum as I spent some time explaining the issue with boolean-base and inheritance based design
3.
  * Additional constraints: TCR and/or Object Calisthenics
  * I was forced to take a break at this session as I had to tackle a person issue during group creation time
4.
  * Language: Haskell
  * Additional constraints: Mute and/or Ping Pong
  * I took another break as there were no one compatible to my setup
  * I decided to do it on my own, but a facilitator came to talk shortly about functional design
  * I ended up completing the implementation of the sum and equivalents in a record of `Int`s
5.
  * Language: Java
  * Paired with a newcomer
  * Additional constraints: Random constraints (I get "no loop" and my pair "TCR")
  * We started by computing CO2 sum (with a `Stream` of enum), then we got stuck trying to compute equivalents
  * We started to introduce `Cost` at the end
6.
  * Language: Haskell
  * Paired with a seasoned crafter
  * I was with another longtime crafter I used to pair with
  * Additional constraints: Primitive Obsessions and/or Evil TDD
  * For a change we started by design `Cost`s (build vs launch)
  * Sadly, my pair was drained and we ended early

## My feedback

I didn't get hooked by the subject:

* I dislike the theme for personal reasons
* The subject was too wide to be addressed in 40 minutes
  * The core domain was too small
  * The Service part was mostly technology-related

I didn't get much from my pairs (which is my fault).

I have given a ROTI of 1/5.
