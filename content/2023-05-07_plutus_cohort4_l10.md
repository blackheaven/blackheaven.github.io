+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 10 - Built with plutus"
date = 2023-05-07
draft = false
path = "2023-05/plutus-c4-l10"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

As a last lecture, instead of writing regular Plutus code through Haskell, alternatives are presented.

First, we have [MeshJS](https://meshjs.dev/), which aims to bootstrap DApps, based on React, NextJS, and JavaScript/TypeScript.

Then come [plu-ts](https://www.npmjs.com/package/@harmoniclabs/plu-ts), which allows tu write scripts which will be compiled down to plutus.

It looks a lot like Haskkell-based Plutus scripts:

```typescript
import { Address, bool, compile, makeValidator, PaymentCredentials, pBool, pfn, Script, ScriptType, V2 } from "@harmoniclabs/plu-ts";

export const contract = pfn([
    AnyDatum.type,
    AnyRedeemer.type,
    V2.PScriptContext.type
],  bool)
(( datum, redeemer, ctx ) =>
    pBool( true )
);


export const untypedValidator = makeValidator( contract );

export const compiledContract = compile( untypedValidator );

export const script = new Script(
    ScriptType.PlutusV2,
    compiledContract
);

export const scriptMainnetAddr = new Address(
    "mainnet",
    new PaymentCredentials(
        "script",
        script.hash
    )
);
```

Then we have [OpShin](https://github.com/OpShin), which is python-based.

```python
from opshin.prelude import *


@dataclass()
class WithdrawDatum(PlutusData):
    pubkeyhash: bytes


def validator(datum: WithdrawDatum, redeemer: None, context: ScriptContext) -> None:
    sig_present = False
    for s in context.tx_info.signatories:
        if datum.pubkeyhash == s:
            sig_present = True
    assert sig_present, "Required signature missing"
```

Then we have [plutarch](https://github.com/Plutonomicon/plutarch-plutus), which is a Haskell eDSL, it aims to produce a smaller and more efficient Untyped PLutus Core:

```haskell
alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \_datm _redm _ctx -> pconstant ()
```

Finally there's [Aiken](https://aiken-lang.org/) which is a dedicated functional programming language for Smart Contracts.

```aiken
validator {
  fn hello_world(datum, redeemer, context) -> Bool {
    let must_say_hello =
      redeemer == "Hello, World!"

    let must_be_signed =
      list.has(
        context.transaction.extra_signatories,
        datum.owner,
      )

    must_say_hello? && must_be_signed?
  }
}
```

Notes: while Plutus Pioneers Program 4th Cohort ends here, the 1st cohort, which I attended to, cover [Plutus Application Backend](https://plutus-apps.readthedocs.io/en/latest/plutus/explanations/pab.html).

Its aims to enable interactions between clients and contracts (e.g. through HTTP requests to long-running contracts, or notify clients when ledger changes).

To be deployed, it takes:
* A chain index
* A node
* A wallet
  * A cardano wallet backend instance (WBE): cardano-wallet
  * A desktop wallet application: Daedalus
  * A browser wallet application: Nami, Yoroi, etc.

