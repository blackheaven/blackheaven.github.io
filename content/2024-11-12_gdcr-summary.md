+++
title = "GDCR 2024: My participation summary"
date = 2024-1102
draft = false
path = "2024-11/gdcr-summary"

[taxonomies]
categories = ["Code practice"]
tags = ["gdcr", "code kata", "coding dojo", "code retreat"]
+++

[As every year](@/2023-11-05_gdcr-summary.md), the [Global day of code retreat](https://www.coderetreat.org/events/) is a good excuse to work and rework the same kata a full day.

This year I have attended to the one organized by the [Software Crafters Lyon](https://twitter.com/swcraftlyon)
(I'm a member of the organizing team, but I didn't organize the Code Retreat this time).

## Subject

It was [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

I have previously attempted it multiple times in the past, but I did not enjoy it.

## My iterations

1. Introduction
  * Language: Kotlin
  * Paired with an experience developer I know for a long time, I wanted to pair with him for a long time, it was his first GDCR/Game of life attempt
  * It was an exploratory session
  * I have started as navigator, setting the design direction to one `data class` per cell state
  * It was uneasy for him, so we switch to a wrapped boolean, eventually coming back to the original design
  * We covered all rules
2. Small method (4 LoC / method) / one level of indentation / ping pong (bonus)
  * Language: Kotlin
  * Paired with a mid-level developer
  * I was the first driver
  * The design was grid-focused (our initial tests was only on one line/column)
  * We made too many big steps
  * We did not cover all rules
3. Primitive Obsession / Blind Navigator / Fluent API (bonus)
  * Language: Haskell
  * Paired with a beginner
  * I mostly lead (which is something I hate doing in GDCR)
  * We focused on the cell
  * We did not cover all rules
4. Test-Commit-Revert (5 minutes) / Branchless
  * Language: Haskell
  * Paired with a mid-level developer (the same of the `2`)
  * We focused on the cell
  * I mostly lead
  * The first commit was a bit long (3m45s), but then we were able to commit every 30-50 seconds
  * We did not cover all rules
5. Evil TDD / Max two parameters
  * Language: Haskell
  * I did this one on my own as I'm not at ease with evil TDD
  * I have tried a branchless design I had in mind for a long time
  * I have focused on the strategies, then the grid
  * I have done the strategies and mostly the grid
6.
  * Language: Ensemble programming + calisthenics + immutable (bonus)
  * Language: Python
  * Paired with two mid-level+ developers and one beginner
  * We started with some grid utilities and finished on strategies
  * We covered all rules

## My feedback

I did not enjoy this day for few reasons:

- I already did this kata too much (probably 65-70 times after this day)
- I was responsible for the note-taking, which did not help to be more focused
- Constraints and sessions were too classical

I have given a ROTI of 2/5.

I had few takeaways:

- Leverage evil TDD to come up with more original designs
- Maybe try another user group next year
