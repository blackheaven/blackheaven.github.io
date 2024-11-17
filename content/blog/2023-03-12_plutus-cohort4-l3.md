+++
title = "Plutus: Pioneers Program 4th cohort - lecture 3: Tests, Times, and Dapps"
date = 2023-03-12
draft = false
path = "2023-03/plutus-c4-l3"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

Following the [Plutus Pioneers Program 1st & 2nd lectures](@/blog/2023-03-05_plutus-cohort4-l1-l2.md), this one takes a step closer to real world use cases.

We are able to tests validators, let's have a look at on of this week tests:

```haskell
good "Deadline: 6000; TxValidRange (6999, 6999)" $ testBeneficiary1 6000 0 0 1
```

`good` comes with `bad` and are design to detect errors:

```haskell
bad msg = good msg . mustFail
good = testNoErrors (adaValue 10_000_000) cfg
```

We can look deeper at the tests definition:

```haskell
testBeneficiary1 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testBeneficiary1 deadline curMinT curMaxT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users
      dat = H1.VestingDatum u1 u2 deadline
  testHomework1 u1 u3 dat curMinT curMaxT wSlot

testHomework1 :: PubKeyHash -> PubKeyHash -> H1.VestingDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework1 sigUser receiver dat curMinT curMaxT wSlot = do
  let val = adaValue 100
  checkBalance (gives sigUser val script1) $ do
    sp <- spend sigUser val
    submitTx sigUser $ vestingTx1 dat sp val
  waitNSlots wSlot
  utxos <- utxoAt script1
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives script1 (txOutValue vestOut) receiver) $ do
    range <- currentTimeInterval curMinT curMaxT
    tx <- validateIn range $ claimingTx1 receiver dat vestRef (txOutValue vestOut)
    submitTx sigUser tx

vestingTx1 :: H1.VestingDatum -> UserSpend -> Value -> Tx
vestingTx1 dat usp val =
  mconcat
    [ userSpend usp
    , payToScript script1 (HashDatum dat) val
    ]

claimingTx1 :: PubKeyHash -> H1.VestingDatum -> TxOutRef -> Value -> Tx
claimingTx1 pkh dat vestRef vestVal =
  mconcat
    [ spendScript script1 vestRef () dat
    , payToKey pkh vestVal
    ]

type Homework1Script = TypedValidator H1.VestingDatum ()

script1 :: Homework1Script
script1 = TypedValidator $ toV2 H1.validator
```

The most interesting part is `testHomework1` which:

* Spend `100 adas` and runs the `vesting` transaction
* Runs the `claiming` transaction:
  * Time boundaries are set
  * Validate the script in the time range
  * Put the validated transaction

Actually, in order for a transaction to be validated regarding time is to check the `ScriptContext` (the third validator argument):

```haskell
data ScriptContext = ScriptContext
    { scriptContextTxInfo :: TxInfo
    , scriptContextPurpose :: ScriptPurpose
    }

data TxInfo = TxInfo
    { txInfoInputs          :: [TxInInfo] -- ^ Transaction inputs
    , txInfoReferenceInputs :: [TxInInfo] -- ^ Transaction reference inputs
    , txInfoOutputs         :: [TxOut] -- ^ Transaction outputs
    , txInfoFee             :: Value -- ^ The fee paid by this transaction.
    , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert           :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl            :: Map StakingCredential Integer -- ^ Withdrawals
    , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories     :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: Map ScriptPurpose Redeemer
    , txInfoData            :: Map DatumHash Datum
    , txInfoId              :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    }
```

`txInfoValidRange` is a field specifying the transaction time validity.

Meaning that, whenever a transaction is sent to a node, the node check it with the availability of `txInfoInputs`.

```haskell
type POSIXTimeRange = Interval POSIXTime

-- | POSIX time is measured as the number of milliseconds since 1970-01-01T00:00:00Z
newtype POSIXTime = POSIXTime { getPOSIXTime :: Integer }

--   The interval can also be unbounded on either side.
data Interval a = Interval { ivFrom :: LowerBound a, ivTo :: UpperBound a }

-- | A set extended with a positive and negative infinity.
data Extended a = NegInf | Finite a | PosInf

-- | Whether a bound is inclusive or not.
type Closure = Bool

-- | The upper bound of an interval.
data UpperBound a = UpperBound (Extended a) Closure

-- | The lower bound of an interval.
data LowerBound a = LowerBound (Extended a) Closure
```

By default, it's set to `-Inf / +Inf`.

In order for transactions to be validated by the node we should be able to craft the transaction and so, putting a time range.

The thing is, Ourobos (the consensus protocol) use slot as time measure (which is currently fixed and set at 1 second, going back and forth is easy).

However it can be changed by a hard fork in the future, meaning that setting a far upper bound may break the contract validation).

Beyond 36 hours, the slot length cannot be guaranteed (hard forks are announced at least 36 hours).

So, you can create a validator script with a far upper bound, but you'll have to create a transactions only few hours before the upper bound's end.

The last part of this lecture is an introduction to _DApps_, which are applications interacting with the Cardano blockchain as they would do for regular APIs.

On another hand, transactions are forged as we would via the `cardano-cli`:

```javascript
async function onVest() {
    const beneficiaryText = document.getElementById('vestBeneficiaryText');
    const beneficiary = beneficiaryText.value;
    const amountText = document.getElementById('vestAmountText');
    const amount = BigInt(parseInt(vestAmountText.value));
    const deadlineText = document.getElementById('vestDeadlineText');
    const deadline = BigInt(Date.parse(deadlineText.value));

    const d = {
        beneficiary: beneficiary,
        deadline: deadline,
    };
    const datum = L.Data.to(d, VestingDatum);
    const tx = await lucid
        .newTx()
        .payToContract(vestingAddress, { inline: datum }, { lovelace: amount })
        .complete();
    signAndSubmitCardanoTx(tx);

    beneficiaryText.value = "";
    amountText.value = "";
    deadlineText.value = "";
}

async function onClaim() {
    const pkh = await getCardanoPKH();

    const referenceText = document.getElementById('claimReferenceText');
    const reference = referenceText.value;

    const utxo = await findUTxO(reference);
    if (utxo) {
        const tx = await lucid
            .newTx()
            .collectFrom([utxo.utxo], L.Data.to(new L.Constr(0, [])))
            .attachSpendingValidator(vestingScript)
            .addSignerKey(pkh)
            .validFrom(Number(utxo.datum.deadline))
            .complete();
        signAndSubmitCardanoTx(tx);
    } else {
        console.log("UTxO not found");
    }

    referenceText.value = "";
}
```

Finally, `lucid`, the library used to interact with the nodes, is initialized upon the `nami` wallet and the _blockfrost_ API service.

```javascript
async function loadCardano() {
    const nami = window.cardano.nami;
    if (!nami) {
        setTimeout(loadCardano);
    } else {
        const api = await nami.enable();
        console.log('nami enabled');
        const lucid = await L.Lucid.new(
            new L.Blockfrost("https://cardano-preview.blockfrost.io/api/v0", "preview1JXEDVldkIyBkxEUrEx3n9ll4afFK1Xj"),
            "Preview",
        );
        console.log('lucid active');
        lucid.selectWallet(api);
        return lucid;
    }
}
```

Even though it's a quick way to get a PoC,  I wouldn't expose my API Key directly.
