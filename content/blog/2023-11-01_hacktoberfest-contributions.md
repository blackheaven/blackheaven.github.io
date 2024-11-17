+++
title = "Hacktoberfest 2023: My contributions"
date = 2023-11-01
draft = false
path = "2023-11/hacktoberfest-contributions"

[taxonomies]
categories = ["FOSS"]
tags = ["foss", "hacktoberfest"]
+++

Following [last year participation](@/blog/2022-11-13_hacktoberfest-contributions.md) I have contributed to [Hacktoberfest](https://hacktoberfest.com/).

Here are my contributions:

- [password](https://github.com/cdepillabout/password): datatypes and functions for easily working with passwords in Haskell
  - Accepted: Introduce CLI
- [nix-output-monitor](https://github.com/maralorn/nix-output-monitor): Pipe your nix-build output through the nix-output-monitor a.k.a nom to get additional information while building.
  - Pending: replace Map with HashMap
- [trial](https://github.com/kowainik/trial): `Either` with events
  - Pending: Add support fort GHC 9.4/9.6
- [essence-of-live-coding](https://github.com/turion/essence-of-live-coding): Universal Live Coding & Functional Reactive Programming Framework
  - Accepted: Add Selective instance to Cell
- [hapistrano](https://github.com/stackbuilders/hapistrano): Deploy tool for Haskell applications, like Capistrano for Rails
  - Accepted: Add interactive init command
- [rhine](https://github.com/turion/rhine): Haskell Functional Reactive Programming framework with type-level clocks
  - Outdated: Add support for dunai 0.11
- [monocle](https://github.com/change-metrics/monocle): Changes-sets organisation and monitoring tool
  - Accepted: Add Workspace name validation
- [ergo](https://github.com/ergochat/ergo): A modern IRC server (daemon/ircd) written in Go
  - Pending: introduce Prometheus
- [OpenSearch documentation](https://github.com/opensearch-project/documentation-website): OpenSearch (ElasticSearch fork) documentation
  - Accepted: drop point in time in 1.3.x 

Which gives:

- Pendings: 3 (- 6)
- Accepted: 5 (- 3)
- Outdated: 1 (- 1)

Note that I went from 19 contributions last year down to 9.

It's due to two main reasons:

- My cat passed away at the beginning of the month, I'm still grieving, my motivation is very low
- The number of active Haskell repositories participating to Hacktoberfest is quite low (I have also tried Go and Rust, but it takes a lot more of efforts)

Also, last year, I have made a contribution to [cabal](https://github.com/haskell/cabal): [Add generate `PackageInfos`](https://github.com/haskell/cabal/pull/8534).

To give a bit of context, in "recent" versions of `cabal`, a module (named `Paths_<package name>`)
is generated, containing functions and bindings related to the package and the
compilation context.

This issue is: when you compile with Nix paths changes, which means that compiled
code importing this module change (even if they don't use these paths), consequently
useless recompilations are performed.

The idea (that was not my idea, an issue was already opened) was to generate another
module (named `PackageInfos_<package name>`), which keeps only stable bindings.

It was merged some time ago, and shipped as part as `cabal` `3.10`.

Sadly, I did not do a proper impacts study and, it occurs that [an error can happen if we try to use it in earlier versions of `cabal`](https://github.com/haskell/cabal/issues/9331).
