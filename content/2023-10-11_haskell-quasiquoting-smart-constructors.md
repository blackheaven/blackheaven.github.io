+++
title = "QuasiQuoting Smart Constructors"
date = 2023-10-11
draft = false
path = "2023-10/haskell-quasiquoting-smart-constructors"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design"]
+++

While I was working on [Bloodhound](https://github.com/bitemyapp/bloodhound),
I have encountered `IndexName`, defined as:

```haskell
newtype IndexName
  = IndexName Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
```

And exported (type and constructor), letting invalid index name to be built.

So, I have hidden the constructor and add a smart constructor enforcing the rules:

```haskell
mkIndexName :: Text -> Either Text IndexName
mkIndexName name = do
  let check explanation p = if p then Right () else Left explanation
  check "Is empty" $ not $ T.null name
  check "Is longer than 255 bytes" $ BS.length (T.encodeUtf8 name) < 256
  check "Contains uppercase letter(s)" $ T.all (\x -> not (isLetter x) || isLower x) name
  check "Includes [\\/*?\"<>| ,#:]" $ T.all (flip @_ @String notElem "\\/*?\"<>| ,#:") name
  check "Starts with [-_+.]" $ maybe False (flip @_ @String notElem "-_+." . fst) $ T.uncons name
  return $ IndexName name
```

It's great, but actually, to ease the testing, we have to come-up with a helper:

```haskell
hardcodedIndexName :: Text -> IndexName
hardcodedIndexName x =
  either
    (\e -> error $ "hardcodedIndexName: cannot make IndexName from " <> show x <> " (" <> show e <> ")")
    id
    $ mkIndexName x
```

Not great, but it can be an issue in production (either you have to reuse the
previous function, with the risk of having unexpected/hard-to-debug errors,
or deal with `Either`).

To avoid that we can use [`QuasiQuotes`](https://wiki.haskell.org/Quasiquotation)
which will allow my to have compile-time errors:

```haskell
testIndex :: IndexName
testIndex = [qqIndexName|-bloodhound-tests-twitter-1|]
```

Which gives:

```
tests/Test/Common.hs:19:13-54: error:
    • Exception when trying to run compile-time code:
        user error ('-bloodhound-tests-twitter-1' is not a valid IndexName (Starts with [-_+.]) at tests/Test/Common.hs:(19,26)-(19,26))
      Code: template-haskell-2.18.0.0:Language.Haskell.TH.Quote.quoteExp
              qqIndexName "-bloodhound-tests-twitter-1"
    • In the quasi-quotation:
        [qqIndexName|-bloodhound-tests-twitter-1|]
   |
19 | testIndex = [qqIndexName|-bloodhound-tests-twitter-1|]
   |
```

Finally, the QuasiQuotes is defined as follows:

```haskell
qqIndexName :: QuasiQuoter
qqIndexName =
  QuasiQuoter
    { quoteExp = \str -> do
        loc <- location
        IndexName n <- runIO $ parseIO mkIndexName loc str
        pure $ AppE (ConE 'IndexName) (LitE (StringL $ T.unpack n)),
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
  where
    parseIO :: (Text -> Either Text a) -> Loc -> String -> IO a
    parseIO p loc str =
      case p $ T.pack str of
        Left err ->
          throwIO $
            userError $
              mconcat
                [ "'",
                  str,
                  "'",
                  " is not a valid IndexName ",
                  "(",
                  T.unpack err,
                  ")",
                  " at ",
                  loc_filename loc,
                  ":",
                  show (loc_start loc),
                  "-",
                  show (loc_start loc)
                ]
        Right a ->
          return a
```

Note: I warmly encourage you to have a look at [Well-Typed's article](https://well-typed.com/blog/2014/10/quasi-quoting-dsls/)
to go further.
