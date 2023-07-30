+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 6 - Testing"
date = 2023-04-09
draft = false
path = "2023-04/plutus-c4-l6"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

I was expecting this lecture for so long: testing.

At the time of writing, testing utils are provided in `Plutus.Model`, as part of [`plutus-simple-model`](https://github.com/geniusyield/plutus-simple-model/tree/d710c4c5400ff7072fe89c337c1cdd0128dc5d99), but are not part of the documentation.

Tests are organized as follows:

```haskell
main :: IO ()
main = defaultMain $ do
    testGroup
      "Test simple user transactions"
      [ good "Simple spend" simpleSpend
      ]
      where
        bad msg = good msg . mustFail
        good = testNoErrors (adaValue 10_000_000) defaultBabbage

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING TRANSACTIONS ----------------------------------------

-- Function to test that a simple transaction works
simpleSpend :: Run Bool
simpleSpend = do
    users <- setupUsers                -- Create 3 users and assign each 1000 lovelaces
    let [u1, u2, u3] = users           -- Give names to individual users
    sendValue u1 (adaValue 100) u2     -- Send 100 lovelaces from user 1 to user 2
    sendValue u2 (adaValue 100) u3     -- Send 100 lovelaces from user 2 to user 3
    isOk <- noErrors                   -- Check that all TXs were accepted without errors
    vals <- mapM valueAt users         -- Read user values
    return $ isOk &&                     -- Check isOk and that all users have correct values
           (vals == fmap adaValue [900, 1000, 1100])
```

We have a Function used to build a `TestTree`, which is on of the base type of `Tasty`:

```haskell
testNoErrors :: Value -> MockConfig -> String -> Run a -> TestTree
```

It takes a `Run a`, which wraps a `State Mock`:

```haskell
newtype Run a = Run (State Mock a)
    deriving newtype (Functor, Applicative, Monad, MonadState Mock)

data Mock = Mock
  { mockUsers       :: !(Map PubKeyHash User)
  , mockAddresses   :: !(Map Address (Set TxOutRef))
  , mockUtxos       :: !(Map TxOutRef TxOut)
  , mockRefScripts  :: !(Map TxOutRef TxOut)
  , mockDatums      :: !(Map DatumHash Datum)
  , mockStake       :: !Stake
  , mockTxs         :: !(Log TxStat)
  , mockConfig      :: !MockConfig
  , mockCurrentSlot :: !Slot
  , mockUserStep    :: !Integer
  , mockFails       :: !(Log FailReason)
  , mockInfo        :: !(Log String)
  , mustFailLog     :: !(Log MustFailLog)
  }
```

Nothing spectacular until you notice `newUser` and `sendValue`, which actually are one of the many actions you can run as `Run`:

```haskell
newUser :: Value -> Run PubKeyHash
sendValue :: PubKeyHash -> Value -> PubKeyHash -> Run ()
withSpend :: PubKeyHash -> Value -> (UserSpend -> Run ()) -> Run ()
submitTx :: PubKeyHash -> Tx -> Run ()
currentSlot :: Run Slot
currentTime :: Run POSIXTime
wait :: POSIXTime -> Run ()
waitUntil :: POSIXTime -> Run ()
noErrors :: Run Bool
valueAt :: HasAddress user => user -> Run Value
refValueAt :: HasAddress user => user -> Run Value
valueAtState :: HasAddress user => user -> Mock -> Value
valueAtStateBy :: HasAddress user => (Mock -> Map TxOutRef TxOut) -> user -> Mock -> Value
getHeadRef :: UserSpend -> TxOutRef
spend :: PubKeyHash -> Value -> Run UserSpend
```

This is the basis of the `Run` EDSL, allowing sequential use-case writing:

```haskell
-- SETUP USERS
(u1, u2) <- setupUsers
-- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
let val = adaValue 100                    -- Define value to be transfered
  dsDatum = OnChain.DatumSwap u2 100
sp <- spend u1 val                        -- Get user's UTXO that we should spend
submitTx u1 $ lockingTx dsDatum sp val          -- User 1 submits "lockingTx" transaction
-- WAIT FOR A BIT
waitUntil waitBeforeConsumingTx
-- USER 2 TAKES "val" FROM VALIDATOR
utxos <- utxoAt swapScript                -- Query blockchain to get all UTxOs at script
let [(ref, out)] = utxos                  -- We know there is only one UTXO (the one we created before)
ct <- currentTimeRad 100                  -- Create time interval with equal radius around current time
tx <- validateIn ct $ consumingTx dsDatum u2 ref (txOutValue out)  -- Build Tx
submitTx u2 tx                            -- User 2 submits "consumingTx" transaction
-- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
[v1, v2] <- mapM valueAt [u1, u2]                     -- Get final balances of both users
unless (v1 == adaValue 900 && v2 == adaValue 1_100) $  -- Check if final balances match expected balances
  logError "Final balances are incorrect"
```

A last part coming from `Plutus.Model` are the transactions which can be sent through `submitTx`:

```haskell
submitTx :: PubKeyHash -> Tx -> Run ()
submitTx pkh tx = void $ sendTx =<< signTx pkh tx
```

Note: I find "funny" that `plutus-simple-model` follows that closely Cardano's model.

There's a lot of helpers to build Transactions:

```haskell
payToScript :: (HasDatum script, HasAddress script) => script -> DatumMode (DatumType script) -> Value -> Tx
loadRefScript :: (IsValidator script) => script -> Value -> Tx
loadRefScriptDatum :: (IsValidator script) => script -> DatumMode (DatumType script) -> Value -> Tx
loadRefScriptBy :: (IsValidator script) => script -> Maybe (DatumMode (DatumType script)) -> Value -> Tx
payToRef :: (HasAddress script, HasDatum script) => script -> DatumMode (DatumType script) -> Value -> Tx
payFee :: Ada -> Tx
spendPubKey :: TxOutRef -> Tx
spendScript :: (IsValidator script) => script -> TxOutRef -> RedeemerType script -> DatumType script -> Tx
spendScriptRef :: (IsValidator script) => TxOutRef -> script -> TxOutRef -> RedeemerType script -> DatumType script -> Tx
refInputInline :: TxOutRef -> Tx
refInputHash :: ToData datum => TxOutRef -> datum -> Tx
collateralInput :: TxOutRef -> Tx
refBoxInline :: TxBox script -> Tx
refBoxHash :: IsValidator script => TxBox script -> DatumType script -> Tx
spendBox :: (IsValidator script) => script -> RedeemerType script -> TxBox script -> Tx
readOnlyBox :: (IsValidator script) => script -> TxBox script -> RedeemerType script -> Tx
modifyBox :: (IsValidator script) => script -> TxBox script -> RedeemerType script -> (DatumType script -> DatumMode (DatumType script)) -> (Value -> Value) -> Tx
userSpend :: UserSpend -> Tx
mintTx :: Mint -> Tx
mintValue :: (ToData redeemer) => TypedPolicy redeemer -> redeemer -> Value -> Tx
validateIn :: POSIXTimeRange -> Tx -> Run Tx
```

You can notice two types:

```haskell
data Tx = Tx
    { txInputs      :: Set.Set TxIn
    , txCollateral  :: Set.Set TxIn
    , txReferenceInputs :: Set.Set TxIn
    , txOutputs     :: [TxOut]
    , txCollateralReturn :: Maybe TxOut
    , txTotalCollateral :: Maybe Ada
    , txMint        :: !Value
    , txFee         :: !Ada
    , txValidRange  :: !SlotRange
    , txMintScripts :: Set.Set (Versioned MintingPolicy)
    , txSignatures  :: Map.Map PubKeyHash (C.KeyPair 'C.Witness C.StandardCrypto)
    , txRedeemers   :: Redeemers
    , txData        :: Map.Map DatumHash Datum -- Miniting scripts redeemers
    , txScripts     :: Map.Map ScriptHash (Versioned Script)
    }

data TxBox script = TxBox
  { txBoxRef   :: TxOutRef
  , txBoxOut   :: TxOut
  , txBoxDatum :: DatumType script
  }
```

As you might guess, `Tx` represents a transaction, while `TxBox` represents script/validator data.

Note that it's quite different of Plutus' `TxInfo`:

```haskell
data TxInfo = TxInfo
  { txInfoInputs :: [TxInInfo]
  , txInfoReferenceInputs :: [TxInInfo]
  , txInfoOutputs :: [TxOut]
  , txInfoFee :: Value
  , txInfoMint :: Value
  , txInfoDCert :: [DCert]
  , txInfoWdrl :: Map StakingCredential Integer
  , txInfoValidRange :: POSIXTimeRange
  , txInfoSignatories :: [PubKeyHash]
  , txInfoRedeemers :: Map ScriptPurpose Redeemer
  , txInfoData :: Map DatumHash Datum
  , txInfoId :: TxId
  }
```

The translation is done in some transactions builder:

```haskell
spendScript ::
  (IsValidator script) =>
  script ->
  TxOutRef ->
  RedeemerType script ->
  DatumType script ->
  Tx
spendScript tv ref red dat = toExtra $
  mempty
    { P.txInputs = S.singleton $ Fork.TxIn ref (Just $ Fork.ConsumeScriptAddress (Just $ Versioned (getLanguage tv) (toValidator tv)) (toRedeemer red) (toDatum dat))
    }
```
