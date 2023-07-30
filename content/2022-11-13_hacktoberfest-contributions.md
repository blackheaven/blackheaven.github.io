+++
title = "Hacktoberfest 2022: My contributions"
date = 2022-11-13
draft = false
path = "2022-11/hacktoberfest-contributions"

[taxonomies]
categories = ["FOSS"]
tags = ["foss", "hacktoberfest"]
+++

This year I decided to contribute a lot to [Hacktoberfest](https://hacktoberfest.com/) for three main reasons:

* I wanted to give back to the community
* I wanted to change my contribution routing to expanded to non-essential/needed ones
* I wanted to be challenged, see how I can handle coming to new codebases

I actually completed my first four accepted PRs on October 2and at 7 PM (CEST), I was proud.

Here are my contributions:

- [pg-entity](https://github.com/tchoutri/pg-entity): A pleasant PostgreSQL database layer for Haskell
  - Accepted: Fix `resource-pool` `>= 0.3` compilation
- [cachix-action](https://github.com/cachix/cachix-action): Build software only once and put it in a global cache
  - Pending: Add flake support
- [flora-server](https://github.com/flora-pm/flora-server): A package index for the Haskell ecosystem
  - Outdated: Restore nix flake
  - Pending: Add nix flake CI
  - Pending: Improve bulk performances
- [get-wasp](https://github.com/wasp-lang/get-wasp): `wasp` installer
  - Outdated: Rework update usecase
- [aeson](https://github.com/haskell/aeson): A fast JSON library
  - Accepted: Add `@since` in documentation
- [hackage-server](https://github.com/haskell/hackage-server): Hackage-Server: A Haskell Package Repository
  - Pending: Add `lastVersion` in listings
- [cabal](https://github.com/haskell/cabal): Official upstream development repository for Cabal and cabal-install
  - Pending: Add generate `PackageInfos`
- [hie-bios](https://github.com/haskell/hie-bios): Set up a GHC API session for various Haskell Projects
  - Pending: Add logs over commands
- [wasp](https://github.com/wasp-lang/wasp): A programming language that understands what a web app is.
  - Pending: Add cli `update` command
- [password](https://github.com/cdepillabout/password): datatypes and functions for easily working with passwords in Haskell
  - Accepted: Add utilities functions
- [nix-output-monitor](https://github.com/maralorn/nix-output-monitor): Pipe your nix-build output through the nix-output-monitor a.k.a nom to get additional information while building.
  - Accepted: Drop `flow`-file operators
- [conferer](https://github.com/ludat/conferer): Configuration management for Haskell
  - Accepted: Fix missing projects CI
- [hemmet](https://github.com/astynax/hemmet): Emmet-like text expansion tool capable to produce HTML, CSS, file trees, that stuff
  - Accepted: Add `hlint` & CI Github Action
- [tomland](https://github.com/kowainik/tomland): Bidirectional TOML serialization
  - Pending: Add `hlint` Github Action
  - Pending: Add `Codec` `Selective` instance
- [iris](https://github.com/chshersh/iris): Haskell CLI Framework supporting Command Line Interface Guidelines
  - Accepted: Add `hlint` Github Action
- [eclair-lang](https://github.com/luc-tielen/eclair-lang): A minimal, fast Datalog implementation in Haskell that compiles to LLVM
  - Accepted: Add `hlint` Github Action

Which gives:

- Pendings: 9
- Accepted: 8
- Outdated: 2


