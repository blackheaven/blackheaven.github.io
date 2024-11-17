+++
title = "Haskell: imports vs exports"
date = 2024-02-14
draft = false
path = "2024-02/haskell-imports-exports"

[taxonomies]
categories = ["Software engineering"]
tags = ["software engineering", "design", "haskell"]
+++

Haskell `module`s are defined with:

* Imports
* Exports
* Extensions/flags
* Code

When I build a `module`, I think in terms of interface, that's why I enumerate
the exports:

```haskell
module Naturals
  ( Nat,
    mkNat,
    toInteger,
  )
  where
```

It has several benefits:

* Put purpose-first
* Detect dead code sooner (ease refactorings)
* Ensure stability (making contracts explicit)

This helps also to create [smart-constructors](https://wiki.haskell.org/Smart_constructors),
a common idiom which uncouple data-representation and data-construction.

For example, `Nat` does not expose the constructor in our example, but `mkNat`
should be used as such:

```haskell
newtype Nat
  = Nat { toInteger :: Integer}
  deriving newtype (Eq, Ord, Show)

mkNat :: Integer -> Maybe Nat
mkNat x =
  if x >= 0
    then Just $ Nat x
    else Nothing
```

So `mkNat` enforces `Nat` invariant, letting us also the freedom to rework
underlying constructors without changing consumers.

On another hand, I tend to let `import`s implicit (unless there are conflicts,
or for clarity, I qualify them for this scenario):

```haskell
-- Instead of
import Data.ByteString (pack)
import Data.Text (unpack)
-- I use
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
```

Which reduces maintenance and diffs.

The rational being that implementations (should) change more frequently than
interfaces.
