+++
title = "Introduction to GHC's Generics"
date = 2024-03-20
draft = false
path = "2024-03/generics-introduction"

[taxonomies]
categories = ["dev"]
tags = ["haskell"]
+++

In my [previous log](@/blog/2024-03-17_aeson-key.md), I have focused on an annoying
[`aeson`](https://hackage.haskell.org/package/aeson), I have mentioned that it
was involving some [GHC's `Generic`s](https://hackage.haskell.org/package/base-4.19.1.0/docs/GHC-Generics.html)
code which were too complex for this log.

I had some feedbacks suggesting that it might be interesting to dive deeper in
this subject, so here I am!

In brief, [GHC's `Generic`s](https://hackage.haskell.org/package/base-4.19.1.0/docs/GHC-Generics.html)
is a mechanism allowing to write code depending on data-types type-level structure.

It is defined as this:

```haskell
class Generic a where
  -- | Generic representation type
  type Rep a :: Type -> Type
  -- | Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- | Convert from the representation to the datatype
  to    :: (Rep a) x -> a
```

`Rep a` gives a representation based on very specialized type (e.g. `M1`, `K1`, `:+:`, etc.),
I have never made sense of it (even after 15-20 derivation mechanisms implementation),
so, instead I rely on examples.

Usually you let the GHC deriving it:

```haskell
data T
  = A0
  | A1 ()
  | A2 { a2f0 :: () }
  | A3 { a3f0 :: (), a3f1 :: () }
  deriving stock (Generic)
```

Let's have a look at `Rep T` in `ghci` (GHC's repl)

```haskell
> :kind! Rep T
Rep T :: Type -> Type
= M1
    D
    (MetaData "T" "Ghci3" "interactive" False)
    ((M1 C (MetaCons "A0" PrefixI False) U1
      :+: M1
            C
            (MetaCons "A1" PrefixI False)
            (M1
               S
               (MetaSel
                  Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
               (K1 R ())))
     :+: (M1
            C
            (MetaCons "A2" PrefixI True)
            (M1
               S
               (MetaSel
                  (Just "a2f0") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
               (K1 R ()))
          :+: M1
                C
                (MetaCons "A3" PrefixI True)
                (M1
                   S
                   (MetaSel
                      (Just "a3f0") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                   (K1 R ())
                 :*: M1
                       S
                       (MetaSel
                          (Just "a3f1") NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                       (K1 R ()))))
```

Overwhelming, I know, let's break this down.

I have the following heuristics: datatype are described by layers of
`SomeType metadataPhantomTypes actualTypes proxy`.

Let's peel the onion:

* `M1 D (MetaData "T" "Ghci3" "interactive" False)` `M1` is a metadata holder (here the context) of some structure (`D` is for datatype)
* `:+:` is the sum type representation (the `|` like in `A0 | A1`)
* `M1 C (MetaCons "A0" PrefixI False) U1` is the metadata of a constructor (`C` is for constructor) without value (`U1`), also `PrefixI` indicate the fixity (i.e. prefix / infix)
* `(M1 S (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy) (K1 R ()))` is a metadata of a record selector (`S`) without "label" (`Nothing`) with a field (`K1 R`) of type unit (`()`)
* `M1 C (MetaCons "A2" PrefixI True) (M1 S (MetaSel (Just "a2f0") NoSourceUnpackedness NoSourceStrictness DecidedLazy) (K1 R ()))` similar to the previous one, except the named "label" (`Just a2f0`)
* `:*:` is the product type representation (it happens when you have multiple values per constructors `A0 Int Char`)

It's quite verbose, but each type is simple by it-self.

Then we have to process them, let's illustrate that with an extract of the
[library](https://hackage.haskell.org/package/pure-sum) I have described in my
previous log:

```haskell
class GToSumText f where
  gToSumText :: f a -> T.Text
```

It will be our `type class` to work on `Generic` types.

It will be used to define generic instances (based on `from` output):

```haskell
instance
  (Generic a, GToSumText (Rep a)) =>
  ToSumText (PureSum a)
  where
  toSumText = gToSumText . from . unPureSum
```

We can start with the top-level case which is not interesting, so we only
extract its value:

```haskell
instance (GToSumText a) => GToSumText (M1 D meta a) where -- base type
  gToSumText (M1 x) = gToSumText x
```

Then we arrive at constructor-level, and since we only need constructor name,
we can stop here and extract it through `symbolVal` which convert a type-level
literal (string) to a value-level `String`:

```haskell
instance (KnownSymbol cntr, EnsureEmpty a) => GToSumText (M1 C ('MetaCons cntr p b) a) where -- constructor
  gToSumText (M1 _) = T.pack $ symbolVal $ Proxy @cntr
```

Then, the last interesting case is the sum type:

```haskell
instance (GToSumText a, GToSumText b) => GToSumText (a :+: b) where -- sum type
  gToSumText (R1 x) = gToSumText x
  gToSumText (L1 x) = gToSumText x
```

Now, what will happen ate runtime, let's take `A0`, which will be converted via
`from` to:

```haskell
M1 -- Datatype
  (L1 -- Left part of the sum type (:+:)
     (M1 -- "A0" constructor
        U1 -- No value
     )
  )
```
The heavy-lifting being done at instance selection.

It works well when you have the value, but when you only have the type, it's a
bit more complex.

For instance, when we had to define `FromJSON (PureSum a)`, we need type's
name for `withText :: String (Value -> Parser a) -> Parser a`

```haskell
instance (FromSumText a, Generic a, GConstructorName (Rep a)) => FromJSON (PureSumWith transformation a) where
  parseJSON = withText (getConst $ (to @a) <$> gConstructorName) pureSumWithParser
```

To do so, we can rely on `Const`, which as a non-operant `Functor` `instance`.

Not, it is not operant because it is a functor over a phantom type

```haskell
newtype Const a b = Const { getConst :: a }

instance Functor (Const m) where
    fmap _ (Const v) = Const v
```

Finally we can simply fetch the literal:

```haskell
class GConstructorName f where
  gConstructorName :: Const String (f a)

instance (KnownSymbol typeName) => GConstructorName (M1 D ('MetaData typeName c i b) a) where -- base type
  gConstructorName = Const $ symbolVal (Proxy @typeName)
```
