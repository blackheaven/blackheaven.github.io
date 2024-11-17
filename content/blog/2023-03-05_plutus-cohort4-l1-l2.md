+++
title = "Plutus: Pioneers Program 4th cohort kick start"
date = 2023-03-05
draft = false
path = "2023-03/plutus-c4-l1-l2"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

For a long time I have been interested in blockchain ecosystem for multiple reasons:
* A wide range of applications
* A very innovative field
* Lots of advanced technical topics (which are considered "exotic" in day-to-day production code)
* An open ecosystem (aligned with my [FOSS contributions](@/pages/foss.md))

Which leds me to take [Plutus Pioneers Program: 1st Cohort](https://github.com/blackheaven/plutus-pioneer-program/tree/black-solutions) two years ago.

My experience with the course was deceiving because:
* it was mostly focused on overcoming Haskell mindset specificities (making the homeworks too simple for me)
* no action from me (I was just watching the lectures, solving homeworks in few minutes, checking them in the playground)

Since I plan to be involved in blockchains in a near future, it was a good opportunity to register for the [4th Cohort](https://github.com/blackheaven/plutus-pioneer-program/tree/fourth-iteration-black) just starting.

Note that it will be a new logs series in which I'll try to document the evolutions of my understanding, I don't intend to provide an uptodate reference of Cardano or Plutus.

Addionally to my [regular look for contributions](@/pages/activities.md), I'm willing to accept Plutus smart contracts suggestions.

Importants links:
* Github: [https://github.com/input-output-hk/plutus-pioneer-program/tree/fourth-iteration](https://github.com/input-output-hk/plutus-pioneer-program/tree/fourth-iteration)
* Gitbook: [https://iog-academy.gitbook.io/plutus-pioneers-program-fourth-cohort/](https://iog-academy.gitbook.io/plutus-pioneers-program-fourth-cohort/) (currently outdated)
* Cardano 101: [https://www.essentialcardano.io/article/what-is-cardano-cardano-101](https://www.essentialcardano.io/article/what-is-cardano-cardano-101)

Main differences compare to the 1st cohort:
* Homeworks are coming with tests: a great point as I was lacking of feedbacks previously, being unsure I was doing the right thing
* Lectures are shorter and more focused
* Nix support dropped in favor in devcontainers: which is understandable but which is too bad since my switch to NixOS

Let's start with some definitions:
* [Era](https://input-output-hk.github.io/cardano-wallet/design/Eras): hard fork changing non-backward-compatible blocks production behavior
* [Phase](https://roadmap.cardano.org/en/): big focus of the Cardano Foundation (with multiple Eras), Voltaire focuses on Governance meaning, making Cardano self-sustainable, independant from IOHK
* [Stake pool](https://docs.cardano.org/learn/stake-pools): Stable [node](https://docs.cardano.org/new-to-cardano/cardano-nodes) responsible of processing transactions going to the ledger
* [Ourobos](https://iohk.io/en/blog/posts/2022/06/03/from-classic-to-chronos-the-implementations-of-ouroboros-explained/): Consensus protocol (from the greek, snake eating its own tail)
* Block: Cryptographically verified set of transactions and previous-block reference
* Slot: 1 second time-slice, for each slot, elected leaders propose blocks, only one of them are selected, others are discarded
* Epoch: 432.000 slots (5 days)
* Elected leaders: elected for 20 slots, due to the proof of stake base of _Ourobos_, the more _stake_ a _stake pool_ manage, the higher share a _stake pool_ manage, the higher chance it has to be elected, they are expected to validate transactions
* Smart-contract: Immutable piece of code which defines the conditions triggering the transaction
* Transaction: changes on the blockchain's stakes
* [Plutus core](https://docs.cardano.org/plutus/learn-about-plutus): Native on-chain script language used by Cardano for _smart-contracts_ (it is compiled from the _Plutus Application_ by the _Plutus Tx Compiler_, _Plutus Tx_, a GHC plug-in)
* [Plutus Application Framework](https://github.com/input-output-hk/plutus-apps#the-plutus-application-framework): basis used for _Plutus Applications_, providing a HTTP and Websocket interface
* Plutus Application Backend: executes off-chain part of an application, provides an HTTP API, manages requests to the node and the wallet while maintaining the application state
* Collateral: monetary deposit used only if the smart contract fails on-chain (supposed to avoid malicious actors)

A transaction is composed of:
* inputs: which are spent by _redeemers_ (which are _Datum_ values)
* outputs: which are scripts validating spending associated currencies, and can come with _Datum_

A _Datum_ looks like [aeson's `Value`](https://hackage.haskell.org/package/aeson-2.1.2.1/docs/Data-Aeson.html#t:Value):

```haskell
data Data
  = Constr Integer [Data]
  | Map [(Data, Data)]
  | List [Data]
  | I Integer
  | B ByteString
```

But, if we have a look at a simple validator script:

```haskell
-- This validator succeeds only if the redeemer is 42
--                  Datum         Redeemer     ScriptContext
mk42Validator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mk42Validator _ r _
    | r == Builtins.mkI 42 = ()
    | otherwise            = traceError "expected 42"
{-# INLINABLE mk42Validator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mk42Validator ||])
```

We can notice that:
* The validator function uses `BuiltinData` (and not `Data`)
* The validator function is `INLINABLE`, to be `compile`d and inlined in `Validator`
* The validator function is a `()`, not a `Bool` or any `Monad`

Actually `BuiltinData` is the _Plutus Core_ equivalent of `Data`, going back and forth via:

```haskell
class ToData (a :: Type) where
    toBuiltinData :: a -> BuiltinData

class FromData (a :: Type) where
    fromBuiltinData :: BuiltinData -> Maybe a

class UnsafeFromData (a :: Type) where
    unsafeFromBuiltinData :: BuiltinData -> a
```

We can strengthen the types using `wrap` (which relies on `UnsafeFromData`):

```haskell
mk42Validator :: () -> Integer -> PlutusV2.ScriptContext -> Bool
mk42Validator _ r _ = traceIfFalse "expected 42" $ r == 42
{-# INLINABLE mk42Validator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mk42Validator ||])
```

You can also create specific types thanks to `unstableMakeIsData`:

```haskell
data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer
```

Let's try a simple validator checking the factorization of a number:

```haskell
--              Datum  Redeemer        ScriptContext
mkValidator :: Integer -> (Integer, Integer) -> PlutusV2.ScriptContext -> Bool
mkValidator t (x, y) _ = shouldNotBeOne x && shouldNotBeOne y && shouldBeFactors
  where shouldNotBeOne :: Integer -> Bool
        shouldNotBeOne n = traceIfFalse "No factor should be 1" $ n /= 1
        shouldBeFactors  = traceIfFalse "Not factors" $ x * y == t
{-# INLINABLE mkValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mkValidator ||])
```

Compiling it to _Plutus Core_ gives something like:

```
{
  "type": "PlutusScriptV2",
  "description": "",
  "cborHex": "5908..."
}
```

CBOR standing for Concise Binary Object Representation and is specified by [RFC 7049](https://tools.ietf.org/html/rfc7049).

Let's submit our contract with a random _Datum_:

```
{"int":42}
```

We can check it works via:

```
$ scripts/query-address.sh $(cat code/Week02/experiments/factoring.addr)
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d1604d8ba3128cd1526b6c68f94f8eddcd83b530dda4560dd327f25ad39164d     0        3000000 lovelace + TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (ScriptDataNumber 42)
```

If we then try to collect (using bob's key address first transaction as collateral and above _TxHash_ as `txin`) with an incorrect _Datum_:

```
$ code/Week02/experiments/collect.sh wrong
# Query the protocol parameters
# Build the transaction
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in the order of the TxIds) failed with:
transaction input 0 (in the order of the TxIds) points to a script hash that is not known.

# Sign the transaction
# Submit the transaction
Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (ExtraneousScriptWitnessesUTXOW (fromList [ScriptHash "c06d15e27842d0ccd6c825a13b84830a14215e01c98a93567f8cdbf8"])))),UtxowFailure (FromAlonzoUtxowFail (ExtraRedeemers [RdmrPtr Spend 0])),UtxowFailure (FromAlonzoUtxowFail (PPViewHashesDontMatch (SJust (SafeHash "abb568f2abaa14e3f59d0feaceb981033fdb16850af8339863a05343d8ac19f6")) (SJust (SafeHash "29f1fbb67e8a3f06db19ae5a5f8c6f51968e16920f3b486925beafd1c3e27391")))),UtxowFailure (UtxoFailure (FromAlonzoUtxoFail (ValueNotConservedUTxO (Value 0 (fromList [])) (Value 3000000 (fromList []))))),UtxowFailure (UtxoFailure (FromAlonzoUtxoFail (BadInputsUTxO (fromList [TxIn (TxId {_unTxId = SafeHash "2d1604d8ba3128cd1526b6c68f94f8eddcd83b530dda4560dd327f25ad39164d"}) (TxIx 0)]))))])
transaction id: ee269f32f5eb8abaf580f3684221c8c82066136e6e9ea7b8455fba61ee53f590
Cardanoscan: https://preview.cardanoscan.io/transaction/ee269f32f5eb8abaf580f3684221c8c82066136e6e9ea7b8455fba61ee53f590
```

if fails right away, without going to the blockchain, while a good _Datum_ works:

```
$ code/Week02/experiments/collect.sh good
Transaction successfully submitted.
transaction id: ee269f32f5eb8abaf580f3684221c8c82066136e6e9ea7b8455fba61ee53f590
Cardanoscan: https://preview.cardanoscan.io/transaction/ee269f32f5eb8abaf580f3684221c8c82066136e6e9ea7b8455fba61ee53f590
```

Updating transactions:

```
$ scripts/query-address.sh $(cat keys/bob.addr)
TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2e1fe0fe79677804a70bff96b249d9912143a3464f4788d7a42b1f772bfb2050     0        10000000000 lovelace + TxOutDatumNone
ee269f32f5eb8abaf580f3684221c8c82066136e6e9ea7b8455fba61ee53f590     0        2701365 lovelace + TxOutDatumNone
$ scripts/query-address.sh $(cat keys/alice.addr)
TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2d1604d8ba3128cd1526b6c68f94f8eddcd83b530dda4560dd327f25ad39164d     1        9996833927 lovelace + TxOutDatumNone
$ scripts/query-address.sh $(cat code/Week02/experiments/factoring.addr)
TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

_Bonus_: At some point I did, mistakenly change the validator, which triggered this error:

```
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in the order of the TxIds) failed with: 
The redeemer pointer: RdmrPtr Spend 0 points to a Plutus script that does not exist.
The pointers that can be resolved are: fromList [(RdmrPtr Spend 0,(Spending (TxIn (TxId {_unTxId = SafeHash "2d1604d8ba3128cd1526b6c68f94f8eddcd83b530dda4560dd327f25ad39164d"}) (TxIx 0)),Nothing,ScriptHash "c06d15e27842d0ccd6c825a13b84830a14215e01c98a93567f8cdbf8"))]
```

In order to detect it, you can recompute it with:

```
$ cardano-cli address build-script --testnet-magic 2 --script-file code/Week02/experiments/factoring.plutus
addr_test1wr5wslr3utlm0vccvveg2z4p0p0jp02fcrwmyeaqqwmglsc6urkf0
$ cat code/Week02/experiments/factoring.addr
addr_test1wrqx690z0ppdpnxkeqj6zwuysv9pgg27q8yc4y6k07xdh7qyreaje
```
