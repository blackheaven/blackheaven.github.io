+++
title = "Plutus: Pioneers Program 4th cohort - Lecture 7 - Marlowe and hosted smart contracts"
date = 2023-04-16
draft = false
path = "2023-04/plutus-c4-l7"

[taxonomies]
categories = ["Haskell", "Blockchain"]
tags = ["haskell", "cardano", "smart contracts"]
+++

While Plutus is the "main" entry-point for smart contracts on Cardano, the only requirement is to produce some Plutus Core, to be run on Cardano.
It's an amazing opportunity since Plutus isn't that easy and error-prone.

It leads to the creation of some alternatives to Plutus:

* [OpShin](https://github.com/OpShin): A Python-based Smart Contracts language
* [Aiken](https://aiken-lang.org/): A dedicate functional programming language for Smart Contracts
* [Marlowe](https://marlowe-finance.io/): A(n) (E)DSL (Haskell/JavaScript) focused on financial Smart Contract

One thing which struck me, is the provide GUI-based builder which looks like [Scratch](https://scratch.mit.edu/about), but it actually express the simplicity of the language.
Actually, it relies on the functional programming trinity embodying program-as-expression, relying on one big type:

```haskell
data Contract 
  = Close
  | Pay Party Payee Token Value Contract
  | If Observation Contract Contract
  | When [Case] Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract
```

On thing you should notice is, except `Close`, each value have `Contract` as last element, which is a kind of continuation-passing-style way to build programs.

There are few interesting concepts:
* `Value`s represent values including volatile/contextual info (e.g. current slot interval, token account balance, previous choices)
* `Observation` which are `Value`s-based comparisons results
* `Action` which can be money deposit, choices between alternatives, contract notification

Here are the associated types:

```haskell
data Timeout
  = TimeParam String
  | POSIXTime Integer
  deriving stock (Show,Generic,Eq)

instance Num Timeout where
  -- ...

data Value
  = AvailableMoney AccountId Token
  | Constant Integer
  | ConstantParam String
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | DivValue Value Value
  | ChoiceValue ChoiceId
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue ValueId
  | Cond Observation Value Value
  deriving stock (Show,Generic,Eq)

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs
  deriving stock (Show,Generic,Eq)

data Action
  = Deposit AccountId Party Token Value
  | Choice ChoiceId [Bound]
  | Notify Observation
  deriving stock (Show,Generic,Eq)

data Payee
  = Account AccountId
  | Party Party
  deriving stock (Show,Generic,Eq)

data Case = Case Action Contract
  deriving stock (Show,Generic,Eq)
```

We can try a very simple token swap token:

```haskell
When
    [Case
        (Deposit
            (Role "Seller")
            (Role "Buyer")
            (Token "" "")
            (Constant 10)
        )
        (Pay
            (Role "Buyer")
            (Party (Role "Seller"))
            (Token "" "T")
            (Constant 1)
            Close
        )]
    1682892000000 Close
```

Which could be explain it as follows:
* When _Buyer_ deposit _10 adas_ to _Seller_
* Then _Seller_ pays _1T_ to _Buyer_

