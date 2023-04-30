+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 9 - Stablecoin"
date = 2023-04-30
draft = false
path = "2023-04/plutus-c4-l9"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

Today's lecture aims to be a wrap-up of previous lectures by implementing a stablecoin.

It's supposed to be the indexed (i.e. synchronized) representation of a real-world currency.

Cardano already hosts multiple stablecoins indexed on the USD: [DJED](https://djed.xyz/) and [USDA](https://www.anzens.com/).

This lecture will present an over-collateralized algorithmic stablecoin which uses a liquidation mechanism to incentivise stability.
Which can be broke down as follows:
* Over-collateralized: it takes an extra (150%) amount of collateral to mint the stable coin
* Algorithimic: it relies on a smartcontracts enforcing stablecoin's price
* Liquididation mechanism to incentivise stability: the extra amount of collateral (in opposition to the minted coin's value) which is used to reward someone when he/she liquidates someone else position
  * Liquidating consists in burning the same amount of token in order get their collateral
  * Liquidating can only be done when the value of the coin and the value locked in the collateral are below a certain threshold
* Stability: it's ensured by miniting more stablecoins when ADA goes up, and add more collaterals or burn stablecoins when ADA goes down

Conceptually, it takes:
* A validator script for the oracle which keeps the value up-to-date
* A minting policy for the collateral, to lock and release collaterals
* A minting policy for the minting of the stablecoin

To deploy the Oracle, we should:
* Mint a NFT
* Actually deploy the oracle validator which is an utxo containing the current ADA/USD rate and the NFT (to identify the oracle)

Then we should deploy the collateral validator and minting policy  as reference scripts in the same transaction.

Moreover, we need a smart contract which allows updating the conversion rate, meaning:
* Inputs previous utxo (NFT + old rate)
* Outputs a new utxo (NFT + new rate)

Then comes the user stable coin minting, which is a transaction containing:
* Inputs: user's collaterals (which will be locked), minting policy as reference script, and the oracle validator as reference input
* Outputs: Collateral validator (an utxo with the collateral, and collateral owner and minted amount as datum), and stables in user's wallet

Burning stablecoin takes a transaction which unlock collaterals and mint stablecoins based:
* User's stablecoins
* The first collateral validator as reference script
* Minting policy as reference script
* The second collateral validator (the utxo with the collateral, and collateral owner and minted amount as datum), which  ensures it's your stablecoins you're burning

However, liquidating a other user's stablecoin is slightly different as it takes:
* Liquidator's stablecoins
* The first collateral validator as reference script
* Minting policy as reference script
* The second collateral validator (the utxo with the collateral, and collateral owner and minted amount as datum)
*  Oracle validator as reference input (with the NFT and the conversion rate)

Doing so gives you access to the other user's collateral.

If we follows a complete example:
* _User 1_ mints 100 stablecoins, which locks a collateral of 150 ADA (a threshold of 150%)
* If the conversion rate drops, then there's an incentive to liquidate _User 1_ position
* _User 2_ will then mint 100 stablecoins, at a lower price, and liquidates _User 1_ position, keeping the rewards
