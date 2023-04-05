+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 5 - Minting"
date = 2023-04-02
draft = false
path = "2023-04/plutus-c4-l5"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

This weeks' lesson is focused on NFT (Non-fungible token), which are "tokens" (non-divisible, duplicable, or copyable).

Cardano contains three elements around NTFs:
* Currency
* Token
* Asset

Defined as follows:

```haskell
newtype CurrencySymbol = CurrencySymbol BuiltinByteString

newtype TokenName = TokenName BuiltinByteString

newtype AssetClass = AssetClass (CurrencySymbol, TokenName)
```

`AssetClass` represents a token.

Actually, `ada`s (and, by extension, `lovelace`s, since `1 ada == 1 000 000 lovelace`) are simply special tokens:

```haskell
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol emptyByteString

adaToken :: TokenName
adaToken = TokenName emptyByteString
```

They are integrated in `Value` such as:

```haskell
newtype Value = Value (Map CurrencySymbol (Map TokenName Integer))
```

Actually, it's important to highlight that, usually scripts don't mint (create) or burnt tokens, but when they do, they have to comply to a _minting policy_.

Like for scripts, _minting policies_ have an address (which is `CurrencySymbol`), and is run when working around tokens.

These _minting policies_ look like _validator scripts_:

```haskell
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

mkWrappedPolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy mkPolicy

myPolicy :: MintingPolicy
myPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedPolicy ||])

myCurrencySymbol :: CurrencySymbol
myCurrencySymbol = currencySymbol myPolicy
```

```haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

More here:

* <https://developers.cardano.org/docs/native-tokens/>

Note: At some points, I tried many variations to solve the homeworks, ie.

```
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, "", 1)] -> True
        _            -> False

    -- or
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> "" == policyTokenName && amt == 1
        _                -> False

    -- or
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == policyTokenName && amt == 1
        _                -> False

{-# INLINABLE policyTokenName #-}
policyTokenName :: TokenName
policyTokenName = ""
```

but since Plutus is a subset of Haskell, I ended-up with errors:

```
Exception: Error: Unsupported feature: Use of == from the Haskell Eq typeclass
Exception: Error: Unsupported feature: Type constructor: GHC.Prim.Char#
Exception: Error: Unsupported feature: Use of fromString on type other than builtin strings or bytestrings: Plutus.V1.Ledger.Value.TokenName
```

sadly, it's at runtime, which is frustrating to me.
