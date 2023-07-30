+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 8 - Staking"
date = 2023-04-23
draft = false
path = "2023-04/plutus-c4-l8"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

We came over [Cardano's staking mechanism](https://cardano.org/stake-pool-delegation/) over and over.

At some point, a reward transaction is emitted and, like `Script` and `Minting`, there are `Validator`s:

```haskell
mkStakeValidator :: () -> ScriptContext -> Bool
mkStakeValidator () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ amount cred >= 1
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
        Just amt -> amt
        Nothing  -> traceError "withdrawal not found"

{-# INLINABLE mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator = wrapStakeValidator mkStakeValidator

stakeValidator :: StakeValidator
stakeValidator = mkStakeValidatorScript $$(PlutusTx.compile [|| mkWrappedStakeValidator ||])
```

If we have a closer look, all `Validator`s are wrappers around `Script`:

```haskell
newtype Validator = Validator Script

mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator

newtype MintingPolicy = MintingPolicy Script

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy

newtype StakeValidator = StakeValidator Script

mkStakeValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> StakeValidator
```

It takes the following steps to be activated:
* Build a stake address from the script (`cardano-cli stake-address build`)
* Build the payment address (`cardano-cli stake-address build`)
* Build stake registration and delegation certificates (`cardano-cli stake-address delegation-certificate` and `cardano-cli stake-address registration-certificate`)
* Build, sign, and submit the registration transaction with certificates and the stake script (`cardano-cli transaction build|sign|submit --certificate-*`)
* Build, sign, and submit the withdrawal transaction with the stake script (`cardano-cli transaction build|sign|submit --withdrawal-*`)
