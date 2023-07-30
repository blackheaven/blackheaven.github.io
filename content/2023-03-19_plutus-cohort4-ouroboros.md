+++
title = "Plutus: Pioneers Program 4th cohort - Ouroboros"
date = 2023-03-19
draft = false
path = "2023-03/plutus-c4-ouroboros"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

_Ouroboros_ is the consensus protocol powering Cardano.

It is the first secured (and proven to be) proof-of-stake (PoS) blockchain protocol.

Unlike proof-of-work (PoW) protocols, only the selected (or elected) leaders (nodes) are building the new block.
The leaders are choosen proportionally based on the number of contributed tokens, it's called _Stake delegation_.
_Stake delegation_ consists in ADA/Stake owners assigning their stakes associated to their ADA to a (stake) pool.
Each time a leader creates a blocks (and is accept, it is rewarded with ADAs).

_Ouroboros_ have had many versions:
* *Classic*: fundation of the protocol, aiming to be energy-efficent and mathematically provable
* *Byzantine Fault Tolerant*: enabling synchronous communication between federated servers (to have more precise and deterministic consensus)
* *Praos*: introduce _epoch_, strengthen security (being more DDoS resistence)
* *Genesis*: allow starting back from _Genesis_
* *Crypsinous*: increase privacy of _Genesis_
* _Chronos_: introduce a secure way to synchronize clocks

An interesting point raise by the paper came from the benchmark they have ran against Bitcoin.
In order to be protected against a hypothetical double spending attacker (for an assurance level of 99.9%) is between 5-10 and 10-16 faster.
Which is suprising for me for few reasons:
* PoS may appear more brittle
* I would expect it to be faster as bitcoin has a block of 10 minutes vs 20 seconds per slots for _Ouroboros_, meaning it takes more slots than blocks to have the same garantees

You can delegate your adas directly from you wallet, stake pools have some interesting caracteristics you want to have a look at:
* _Saturation_: even though you'd want to delegate to a stake closer to saturation (to earn more), at some point, you don't get any more rewards.
* _Rank_: it gives a good indication sinceit's herarchized against the potential long-term earnings.
* _Live Stake_: Percentage of the total stake in the blockchain controlled by the pool.
* _Pool margin_: The "tax" taken away by the pool on each reward.
* _Pledge_: amount of stakes stake pool owners delegate to their own pool
* _Cost per epoch_: fixed fee per epoch charged by the stake pool to cover its operating costs.
* _Produced blocks_: number of blocks produced by the stake pool since the beginning

References:
* [https://iohk.io/en/research/library/papers/ouroboros-a-provably-secure-proof-of-stake-blockchain-protocol/](https://iohk.io/en/research/library/papers/ouroboros-a-provably-secure-proof-of-stake-blockchain-protocol/)
* [https://www.vanticatrading.com/post/what-is-ouroboros-the-cardano-consensus-protocol](https://www.vanticatrading.com/post/what-is-ouroboros-the-cardano-consensus-protocol)
* [https://docs.cardano.org/learn/ouroboros-overview](https://docs.cardano.org/learn/ouroboros-overview)
* [https://docs.cardano.org/new-to-cardano/how-to-delegate](https://docs.cardano.org/new-to-cardano/how-to-delegate)
* [https://iohk.io/en/blog/posts/2022/06/03/from-classic-to-chronos-the-implementations-of-ouroboros-explained/?utm_source=pocket_reader](https://iohk.io/en/blog/posts/2022/06/03/from-classic-to-chronos-the-implementations-of-ouroboros-explained/?utm_source=pocket_reader)
