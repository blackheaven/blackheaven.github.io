+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 4 - On-chain/Off-chain smart contract code and indexers"
date = 2023-03-26
draft = false
path = "2023-03/plutus-c4-l4"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

Cardano smart contract's code is splitted in two parts: *on-chain* and *off-chain*

*On-chain* code is executed during transaction/data blockchain incorporation, on blockchain's side.
* Has to be synchronized between all nodes (to ensure integrity, which is costly)
* Follows a complex compilation process:
  * PlutusTx: used by developpers (subset of Haskell/GHC)
  * GHC Core
  * Plutus Intermediate Representation (PIR)
  * Plutus Core (PLC)
  * Untyped Plutus Core (UPLC): run on the blockchain (stored in CBOR)

*Off-chain* code which run to build/submit transactions, on the user's side.
* Accesses wallet and resources
* Is favored (since complex *on-chain* will end up slowing down the whole blockchain)
* Queries the blockchain (via `cardano-cli query tip|utxo|...`)
  * Used for validators execution (transaction context, inputs/outputs with Values/Datums/Scripts, and our redeemer)
  * Fetch this information from
    * A Local node
    * A local database synchronized with
      * A local node
      * A remote indexer

There are many indexers:
* [db-sync](https://github.com/input-output-hk/cardano-db-sync): PostGreSQL-based synchronization (allows to qury directly using [SQL](https://github.com/input-output-hk/cardano-db-sync/blob/master/doc/interesting-queries.md) in read-only)
  * According to [the documentation](https://docs.cardano.org/cardano-components/cardano-db-sync) it also provides frontends:
    * A GraphQL API Server (Apollo)
    * A REST API Server
    * A [SMASH](https://docs.cardano.org/cardano-components/smash) (Stake Pool Metadata Aggregation Server) aggregating off-chain blockchain's stake pools' metadata
  * It requires a local Cardano node
* [Kupo](https://github.com/CardanoSolutions/kupo): indexes only outputs, providing a fast, high-level HTTP/JSON API
  * It is configurable such as you can only index a sub-part of all transactions
  * It can be run in-memory
  * It requires a local Cardano node
* [Scrolls](https://github.com/txpipe/scrolls): Meta-indexer which relies on plugins to provide read-optimized models
  * Inputs plugins: Cardano node, Block CBOR Files
  * Outputs plugins: Redis, MongoDB, Cassandra, etc.
  * Quite early-stage at the moment
* [Carp](https://github.com/dcSpark/carp): Another PostGreSQL-based indexer (Cardano Postgres Indexer), relying on *Oura*
  * Provides a REST API
  * Parses raw CBOR data and stores them in PostGreSQL
  * Is highly-configurable (based on [execution plans](https://dcspark.github.io/carp/docs/indexer/example_plan))
* [Oura](https://github.com/txpipe/oura): Acts as `tail` on the transactions
  * Connects to a Cardano node
  * Outputs to Webhooks/Kafka/ElasticSeearch/stdout
  * Is configurable (filter)
* [Marconi](https://github.com/input-output-hk/marconi): it is supposed to be lightweight, customizable for indexing and querying Cardano
  * It requires a local Cardano node
  * Based on multiple SQLite files
  * Can index either
    * the blockchain (though [marconi-chain-index](https://github.com/input-output-hk/marconi/tree/main/marconi-chain-index)), accessible via a cli
    * or sidechains (though [marconi-sidechain](https://github.com/input-output-hk/marconi/tree/main/marconi-sidechain)), accessible via a HTTP JSON-RPC service

Some of the indexers are written in Rust, based on the [CML](https://github.com/dcSpark/cardano-multiplatform-lib) library which aims to partially rewrite Cardano ecosystem.
