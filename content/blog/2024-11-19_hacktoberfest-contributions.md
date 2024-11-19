+++
title = "Hacktoberfest 2024: My contributions"
date = 2024-11-19
draft = false
path = "2024-11/hacktoberfest-contributions"

[taxonomies]
categories = ["FOSS"]
tags = ["foss", "hacktoberfest"]
+++

Following [last year participation](@/blog/2023-11-01_hacktoberfest-contributions.md) I have contributed to [Hacktoberfest](https://hacktoberfest.com/).

Here are my contributions:

- [password](https://github.com/cdepillabout/password): datatypes and functions for easily working with passwords in Haskell
  - Accepted: introduce golden tests
- [eselsohr](https://github.com/mkoppmann/eselsohr): self-hostable bookmark manager for storing web articles
  - Accepted: fix nix compilation
- [nixpkgs](https://github.com/NixOS/nixpkgs): Nix packages collection & NixOS
  - FZF_ALT_C_COMMAND: `nixos/minidlna`: add package option
  - Pending: `nixos/vivid`: init module
- [hapistrano](https://github.com/stackbuilders/hapistrano): Deploy tool for Haskell applications, like Capistrano for Rails
  - Accepted: add `--dry-run` flag
- [servant-static-th](https://github.com/cdepillabout/servant-static-th): embed static directories in `servant`
  - Accepted: fix compilation
- [servant](https://github.com/haskell-servant/servant): strongly-typed web server
  - Accepted: recommend argon2 in JWT cookbook
- [nix-tree](https://github.com/utdemir/nix-tree): nix derivation dependency browser
  - Accepted: add `--file` flag
- [crem](https://github.com/marcosh/crem): Haskell composable state-machine
  - Accepted: add nothunks instances
  - Pending: fix nix compilation

Which gives:

- Pendings: 2 (- 1)
- Accepted: 7 (+ 2)
- Outdated: 0 (- 1)

My number of contributions has been stable compared to last year, which is remarkable since:

- I didn't anticipated Hacktoberfest unlike previous years (finding some issue, starting local drafts, etc.), thanks for my colleagues for pointing that out
- I was in vacations for the most of October, which means that most of the contributions have been done over a weekend and two nights

Unlike previous years, most of my contributions were not in Haskell
(4 in Haskell, 4 in Nix, 1 documentation), it's not that I'm no longer interested
in Haskell, it comes from the fact that most of the GitHub Haskell projects
with "beginners" issues are not actively maintained.

I also note that it's really hard to get things merged in `nixpkgs`, which isn't a good idea for Hacktoberfest.
