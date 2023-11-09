+++
title = "FOSS Contributions"
path = "foss"
template = "pages.html"
draft = false
+++


# Open sources contributions

Used as [my cookie jar](https://thewilliamkon.medium.com/david-goggins-the-cookie-jar-method-explained-fa2f6d586d50)

## Haskell (hackage) maintained libraries

I have published some packages over Hackage (Haskell):

- [kill-bool](https://hackage.haskell.org/package/kill-bool) (March 2023): Booleans strong-typing
- [poolboy](https://hackage.haskell.org/package/poolboy) (March 2023): Simple work queue for bounded concurrency
- [adhoc-fixtures](https://hackage.haskell.org/package/adhoc-fixtures) (November 2022): Simplify tests fixtures (also [-hspec](https://hackage.haskell.org/package/adhoc-fixtures-hspec))
- [yarl](https://hackage.haskell.org/package/yarl) (September 2022): simple extensible records library (with `getField` support)
- [sized-wrapper](https://hackage.haskell.org/package/sized-wrapper) (August 2022): provides a wrapper for sized values (also [-aeson](https://hackage.haskell.org/package/sized-wrapper-aeson), [-quickcheck](https://hackage.haskell.org/package/sized-wrapper-quickcheck), [-text](https://hackage.haskell.org/package/sized-wrapper-text))
- [librarian](https://hackage.haskell.org/package/librarian) (July 2022): a rules-based files moving cli tool
- [nonempty-wrapper](https://hackage.haskell.org/package/nonempty-wrapper) (June 2022): provides a wrapper for non-empty values (also [-aeson](https://hackage.haskell.org/package/nonempty-wrapper-aeson), [-quickcheck](https://hackage.haskell.org/package/nonempty-wrapper-quickcheck), [-text](https://hackage.haskell.org/package/nonempty-wrapper-text))
- [pulsar-hs](https://github.com/hetchr/pulsar-hs) (September 2021): complete [Apache Pulsar](https://github.com/apache/pulsar) client and admin client
- [aeson-modern-tojson](https://hackage.haskell.org/package/aeson-modern-tojson) (June 2021): aims to give a post 0.x way to derive aeson's ToJSON instances
- [typed-time](https://hackage.haskell.org/package/typed-time) (May 2021): provides a type-level time format
- [amqp-streamly](https://hackage.haskell.org/package/amqp-streamly) (August 2020): Connect amqp (RabbitMQ) to streamly
- [servant-streamly](https://hackage.haskell.org/package/servant-streamly) (July 2020) Streamly support for Servant
- [dijkstra-simple](https://hackage.haskell.org/package/dijkstra-simple) (June 2020): a simple Dijkstra's shortest-path implementation

## Various maintained projects

- [docker-soju](https://github.com/blackheaven/docker-soju) (Docker) (August 2022): Dockerized version of soju (an IRC bouncer)
- [coc-hls](https://www.npmjs.com/package/coc-hls) (npm) (December 2021): haskell-language-server coc.nvim extension
- [danger-ormolu](https://rubygems.org/gems/danger-ormolu) (RubyGems) (August 2021): ormulu integration in Danger (CI tool)

## Other contributions:

- [essence-of-live-coding](https://github.com/turion/essence-of-live-coding): Universal Live Coding & Functional Reactive Programming Framework
  - October 2023: Add Selective instance to Cell
- [hapistrano](https://github.com/stackbuilders/hapistrano): Deploy tool for Haskell applications, like Capistrano for Rails
  - October 2023: Add interactive init command
- [rhine](https://github.com/turion/rhine): Haskell Functional Reactive Programming framework with type-level clocks
  - October 2023: Add support for dunai 0.11
- [monocle](https://github.com/change-metrics/monocle): Changes-sets organisation and monitoring tool
  - October 2023: Add Workspace name validation
- [ergo](https://github.com/ergochat/ergo): A modern IRC server (daemon/ircd) written in Go
  - October 2023: introduce Prometheus
- [OpenSearch documentation](https://github.com/opensearch-project/documentation-website): OpenSearch (ElasticSearch fork) documentation
  - October 2023: drop point in time in 1.3.x
- [skip-duplicate-actions](https://github.com/blackheaven/skip-duplicate-actions): Github Action to skip jobs on arbitrary conditions
  - September 2023: Upgrade to NodeJS 20
- [lyontechhub.github.io](https://github.com/lyontechhub/lyontechhub.github.io): LyonTechHub (Community of tech communities of Lyon)
  - August 2023: Migrate from Angular to static site generation
- [dhall-openssl](https://github.com/jvanbruegge/dhall-openssl): `dhall` types and utils to generate `OpenSSL`/`LibreSSL` configuration files
  - August 2023: Add CRL support
- [microlens-aeson](https://github.com/fosskers/microlens-aeson): `microlens` for `aeson`
  - August 2023: Add `aeson` `2.2.0.0` support
- [nixpkgs](https://github.com/NixOS/nixpkgs): nix-based packages set
  - June 2023: upgrade `curlie` to `1.7.1`
  - July 2023: add `meta.mainProgram` to eventstore
- [curlie](https://github.com/rs/curlie): `curl` Rust wrapper (like `httpie`)
  - May 2023: Fix `curl` long flags with value
- [purebred-email](https://github.com/purebred-mua/purebred-email): Haskell MIME e-mails parsing and manipulation library
  - March 2023: Fix documentation
- [tmux-net-speed](https://github.com/tmux-plugins/tmux-net-speed): tmux status bar net speed fragment
  - March 2023: Fix `bash` invocation
- [cardano-documentation](https://github.com/input-output-hk/cardano-documentation): Cardano blockchain documentation
  - March 2023: Fix a typo
- [pg-entity](https://github.com/tchoutri/pg-entity): A pleasant PostgreSQL database layer for Haskell
  - October 2022: Fix `resource-pool` `>= 0.3` compilation
- [cachix-action](https://github.com/cachix/cachix-action): Build software only once and put it in a global cache
  - October 2022: Add flake support
- [flora-server](https://github.com/flora-pm/flora-server): A package index for the Haskell ecosystem
  - October 2022: Restore nix flake
  - October 2022: Add nix flake CI
  - October 2022: Improve bulk performances
  - March 2023: Improve bulk performances
- [get-wasp](https://github.com/wasp-lang/get-wasp): `wasp` installer
  - October 2022: Rework update usecase
- [aeson](https://github.com/haskell/aeson): A fast JSON library
  - October 2022: Add `@since` in documentation
- [hackage-server](https://github.com/haskell/hackage-server): Hackage-Server: A Haskell Package Repository
  - October 2022: Add `lastVersion` in listings
  - February 2023: Fix `lastVersion` update in listings
- [cabal](https://github.com/haskell/cabal): Official upstream development repository for Cabal and cabal-install
  - October 2022: Add generate `PackageInfos`
- [hie-bios](https://github.com/haskell/hie-bios): Set up a GHC API session for various Haskell Projects
  - October 2022: Add logs over commands
- [wasp](https://github.com/wasp-lang/wasp): A programming language that understands what a web app is.
  - October 2022: Add cli `update` command
- [password](https://github.com/cdepillabout/password): datatypes and functions for easily working with passwords in Haskell
  - October 2022: Add utilities functions
  - October 2023: Introduce CLI
- [nix-output-monitor](https://github.com/maralorn/nix-output-monitor): Pipe your nix-build output through the nix-output-monitor a.k.a nom to get additional information while building.
  - October 2022: Drop `flow`-file operators
  - October 2023: replace Map with HashMap
- [conferer](https://github.com/ludat/conferer): Configuration management for Haskell
  - October 2022: Fix missing projects CI
- [hemmet](https://github.com/astynax/hemmet): Emmet-like text expansion tool capable to produce HTML, CSS, file trees, that stuff
  - October 2022: Add `hlint` & CI Github Action
- [tomland](https://github.com/kowainik/tomland): Bidirectional TOML serialization
  - October 2022: Add `hlint` Github Action
  - October 2022: Add `Codec` `Selective` instance
- [iris](https://github.com/chshersh/iris): Haskell CLI Framework supporting Command Line Interface Guidelines
  - October 2022: Add `hlint` Github Action
- [eclair-lang](https://github.com/luc-tielen/eclair-lang): A minimal, fast Datalog implementation in Haskell that compiles to LLVM
  - October 2022: Add `hlint` Github Action
- [spaceship-vi-mode](https://github.com/spaceship-prompt/spaceship-vi-mode/): vi-mode plugin for `spaceship` (a Zsh customisable prompt)
  - September 2022: Fix `antigen` deploy instructions
- [zinza](https://hackage.haskell.org/package/zinza): `Jinja` style templating library for Haskell
  - September 2022: Add `DerivingVia` `Generic`s `newtype`s
- [event-streaming-patterns](https://github.com/confluentinc/event-streaming-patterns): Confluent/Kafka Messaging/Streaming patterns
  - August 2022: Fix code example
- [hspec](https://hackage.haskell.org/package/hspec): `hspec` is a behavior-driven development testing library (à la RSpec)
  - August 2022: Add parallel result rendering
- [cabal](https://github.com/haskell/cabal): Haskell main build tool
  - July 2022: Clarify cli flag documentation
- [mercure](https://mercure.rocks): Server-Sent Event hub
  - July 2022: Add Caddy directive for Docker deployment
- [hspec-discover](https://hackage.haskell.org/package/hspec-discover): `hspec`s driver for tests discovery
  - June 2022: Add parallel runner
- [optparse-generic](https://hackage.haskell.org/package/optparse-generic): `optparse-application` `Generic`s based library
  - April 2022: Add `Parser`/`Record` runner
- [wai-extra](https://hackage.haskell.org/package/wai-extra): WAI utilities
  - April 2022: Add `Middleware`s for health checks
  - April 2022: Add a dynamic `Middleware` builder
- [vim-slime](https://github.com/jpalardy/vim-slime): ViM plugin for interactions with terminal multiplexers
  - December 2021: Fix tmux support
- [powerline-go](https://github.com/justjanne/powerline-go): Powerline-like shell prompts customization
  - December 2021: Add vi-mode segment
- [tmux-mem-cpu-load](https://github.com/thewtex/tmux-mem-cpu-load): tmux monitor (CPU, RAM, load) utils (for the status line)
  - December 2021: Change RAM units threshold and CPU usage color
- [Apache Pulsar](https://github.com/apache/pulsar): distributed pub-sub messaging system
  - November 2021: Headers fixes and synchronous functions addition in the C client
- [generic-random](https://hackage.haskell.org/package/generic-random): derive via GHC.Generics QuickCheck's Arbitrary instance
  - July 2021: Implement a type-level deriving via module
- [bloodhound](https://hackage.haskell.org/package/bloodhound): ElasticSearch client
  - Since April 2022: Maintainer
  - February 2021: bug fixes
- [pandoc](https://hackage.haskell.org/package/pandoc): text format processing
  - January 2021: Support for Github wikilinks
- [testcontainers-hs](https://hackage.haskell.org/package/testcontainers): manage Docker containers for tests
  - June 2022: Add naming strategies
  - May 2021: bug fixes
  - June-August 2020: Hspec support, bug fixes, aeson-optics usage
- [hatrace](https://github.com/nh2/hatrace): strace-like
  - June 2020: Introduction of many syscalls
