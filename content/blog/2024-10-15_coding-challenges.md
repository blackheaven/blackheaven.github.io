+++
title = "Coding challenges retrospective"
date = 2024-10-15
draft = false
path = "2024-10/coding-challenges"

[taxonomies]
categories = ["Code practice"]
tags = ["retrospective", "learning"]
+++

Over the last few months, I have practiced [coding challenges](@/blog/2024-03-24_streak.md) ([here](https://github.com/blackheaven/roadmap.rs)).

I had two goals in mind:

* Improving my Rust mastery
* Go deep into the internals and concepts of my day-to-day tools

I had many projects such as:

* Building a REST API (with `axum` then `rocket.rs`) with a TODO List/Kanban
* Using WASM to build a browser (client-side) Hangman
* Play with the FFI build a CLI-based SQLite app
* Playing with concurrent IO (`tokio`) implementing `redis`/`memcached` clones
* Reading/writing regular format (`xxd`/`zip` clones)
* Playing with protocol (a DNS client)
* Dealing with kernel calls (`docker` clone)
* Implement some distributed systems tactics (rate limiter, reverse proxy)

I have learned few interesting things, such as:

* DNS is actually a [very complex](https://www.rfc-editor.org/rfc/rfc9499.html) protocol
* `tar` stores length in [`octal`](https://en.wikipedia.org/wiki/Tar_(computing)#Header)
* GNU `tar` implements [UStar](https://en.wikipedia.org/wiki/Tar_(computing)#UStar_format)
* Knowing that you can implements a tool is really different from implementing it, it helped me to gain a lot of confidence in my skill
* Even quick and dirty code can be a first step and work enough to be a good proof-of-concept
