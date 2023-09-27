+++
title = "Witness functions"
date = 2023-09-27
draft = false
path = "2023-09/witness-functions"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "engineering", "refactoring"]
+++

One of my favorite features of Haskell are the (G)ADTs, we could for example define a
(not really) useful one:

```haskell
data Expr
  = Val Int
  | Add Expr Expr
```

Doing so helps to easily define self-contained logic

```haskell
eval :: Expr -> Int
eval =
  \case
    Val x -> x
    Add x y -> eval x + eval y
```

Usually we have write functions around them which go back and forth:

```haskell
instance FromJSON Expr where
  parseJSON =
    withObject "Expr" $ \o -> do
      op <- o .: "op"
      case op of
        "val" -> Val <$> o .: "value"
        "add" -> Add <$> o .: "op0" <*> o .: "op1"
        _ -> fail $ "Unknown operator: '" <> op <> "'"

instance ToJSON Expr where
  toJSON =
    \case
      Val x -> object ["op" .= ("val" :: Text), "value" .= x]
      Add x y -> object ["op" .= ("add" :: Text), "op0" .= x, "op1" .= y]
```

If I add or change the constructors, I'll have a warning (or an error if properly configured)
on `ToJSON` instance, hopefully I'll a test covering that, or, if piece of code are located
next to each other, I'll think of modifying `FromJSON`.

But sometimes I have "one-way" functions:

```haskell
example :: Text -> Maybe Expr
example =
  \case
    "val" -> Just $ Val 42
    "add" -> Just $ Add (Val 1) (Val 1)
    _ -> Nothing
```

Adding new constructors won't generate any warnings/errors, and tests won't catch it.

In these situations, I add a witness function below whose only purpose is to make
the compilation break, forcing me to change the given function:

```haskell
_witnessExample :: Expr -> ()
_witnessExample =
  \case
    Val _ -> ()
    Add _ _ -> ()
```
