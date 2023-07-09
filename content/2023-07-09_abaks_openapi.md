+++
title = "Abaks: OpenAPI"
date = 2023-07-09
draft = false
path = "2023-07/abaks-openapi"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "project", "abaks"]
+++

The last thing we have to provide is an OpenAPI description.

Thanks to Haskell's type system and `servant-openapi3`, we can generate it, by first annotating our API types:

```haskell
data EntryA = EntryA
  { entryId :: Int,
    amount :: AmountA,
    category :: Text,
    comment :: Text,
    state :: EntryStateA,
    date :: Day
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via (DerivingAPIProduct EntryA)
```

You can notice `ToSchema` which will be used to generate the OpenAPI description.

Regarding the generation itself, I tend to add a dedicated endpoint, but this time I'll create a dedicated binary:

```haskell
abaksOpenAPI :: OpenApi
abaksOpenAPI =
  toOpenApi (Proxy @API)
    & info . title .~ "Abaks API"
    & info . version .~ "1.0"
    & info . description ?~ "This API allows interacting with Abaks"
    & info . license ?~ ("ISC" & url ?~ URL "https://opensource.org/license/isc-license-txt/")

main :: IO ()
main = BL8.putStrLn $ encodePretty abaksOpenAPI
```

Pretty simple since all the "heavy-lifting" has been done with types.

Which gives us [a decent result](/posts/2023-07-09_abaks_openapi/api.html):

![OpenAPI view](/posts/2023-07-09_abaks_openapi/api.png)

Not only OpenAPI will give a way to have an always valid description, but you'll also be able to generate some interfaces for the clients.
