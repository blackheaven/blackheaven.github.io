+++
title = "Test-Driven Development Freedom"
date = 2023-08-13
draft = false
path = "2023-08/tdd-freedom"

[taxonomies]
categories = ["dev"]
tags = ["haskell", "design", "engineering", "test-driven development"]
+++

In [My previous log](@/blog/2023-08-09_good-design.md) I mentioned my bad intuition regarding code design.

Let's task a real-world example.

I have written [my log on Caddy](@/blog/2023-07-26_caddy.md) after working with [Mercure](https://mercure.rocks/), which was used to push updates to users.

In a nutshell, _Mercure_ is a hub which exposes an API to push messages on _topics_, each _topic_ being able to be subscribed to by client (users' browser).

At some point, we wanted to approximate the online users (and their current interests).

Each topic having the following shape: `/foo/bar/baz`, we want to match partial topic (i.e. `/topic/{topicId}/edit`).
Note that matcher and topic have to have the same length (i.e. `/topic/{topicId}` matches `/topic/42`, but not `/topic/42/edit`).

Intuitively, we can set up the following types:

```haskell
data Subscription = Subscription
  { id :: SubscriptionId,
    topics :: [Topic]
  }
  deriving stock (Eq, Show)

newtype SubscriptionId 
  = SubscriptionId { getSubscriptionId :: Text }
  deriving stock (Eq, Show)

newtype TopicPart 
  = TopicPart { getTopicPart :: Text }
  deriving stock (Eq, Show)

newtype Topic 
  = Topic { getTopic :: [TopicPart] }
  deriving stock (Eq, Show)

data TopicMatcherPart 
  = TMExact Text
  | TMWildcard
  deriving stock (Eq, Show)

newtype TopicMatcher 
  = TopicMatcher { getTopicMatcher :: [TopicMatcherPart] }
  deriving stock (Eq, Show)

subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = undefined
```

Then, here is what I had in mind:

```haskell
subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = any (flip subscriptionMatches s) ms

subscriptionMatches :: TopicMatcher -> Subscription -> Bool
subscriptionMatches m s = any (topicMatches m) s.topics

topicMatches :: TopicMatcher -> Topic -> Bool
topicMatches m t = length m.getTopicMatcher == length t.getTopic && and (zipWith match m.getTopicMatcher t.getTopic)
  where match mp tp =
          case mp of
            TMExact e -> e == tp.getTopicPart
            TMWildcard -> True
```

It's a bit big, let's restart in [Test driven development](https://martinfowler.com/bliki/TestDrivenDevelopment.html).

Let's set up our first test:

```haskell
spec :: Spec
spec =
  describe "Matcher" $ do
    forM_
      [ (["/"], ["/"], True)
      ]
      $ \(matchers, topics, matches) ->
        it (show matchers <> " ~= " <> show topics <> " => " <> show matches) $ do
          let rawParts = tail . T.splitOn "/"
              mkMatcherPart x = if x == "*" then TMWildcard else TMExact x
              matchers' = TopicMatcher . map mkMatcherPart . rawParts <$> matchers
              subscription' = Subscription (SubscriptionId "...") (Topic . map TopicPart . rawParts <$> topics)
          subscriptionsMatches matchers' subscription' `shouldBe` matches
```

it's red, we can pursue with a naive implementation:

```haskell
subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = True
```

Then with unmatching urls:

```haskell
-- (["/a"], ["/b"], False)
subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = any (flip subscriptionMatches s) ms

subscriptionMatches :: TopicMatcher -> Subscription -> Bool
subscriptionMatches m s = any (topicMatches m) s.topics

topicMatches :: TopicMatcher -> Topic -> Bool
topicMatches m t = map matcherString m.getTopicMatcher == map (.getTopicPart) t.getTopic
  where
    matcherString =
      \case
        TMExact e -> e
        TMWildcard -> "*"
```

A lot of heavy-lifting, as we are constrained by the types:

We can then add wildcards:

```haskell
-- (["/a/*"], ["/a/c"], True)
topicMatches :: TopicMatcher -> Topic -> Bool
topicMatches m t = and (zipWith match m.getTopicMatcher t.getTopic)
  where
    match mp tp =
      case mp of
        TMExact e -> e == tp.getTopicPart
        TMWildcard -> True
```

Well, we nearly reached initial implementation, except, there's a regression now:

```haskell
-- (["/a/*"], ["/a/c/e"], False)
topicMatches :: TopicMatcher -> Topic -> Bool
topicMatches m t = length m.getTopicMatcher == length t.getTopic && and (zipWith match m.getTopicMatcher t.getTopic)
  where
    match mp tp =
      case mp of
        TMExact e -> e == tp.getTopicPart
        TMWildcard -> True
```

We get back to our initial implementation, except, now we are able to make experiment.

Especially, we would like to find a function which only work when to "containers" have the same size:

```haskell
_ :: f ~ [] => (a -> b -> Bool) -> f a -> f b -> Bool
```

After a quick look at Hoogle:

```haskell
liftEq :: Eq1 f => (a -> b -> Bool) -> f a -> f b -> Bool
```

Let's try that:

```haskell
topicMatches :: TopicMatcher -> Topic -> Bool
topicMatches m t = liftEq match m.getTopicMatcher t.getTopic
  where
    match mp tp =
      case mp of
        TMExact e -> e == tp.getTopicPart
        TMWildcard -> True
```

Here we are, we got a neater implementation, focused on the matching, not the data-structures.

That's one of the reason I go on _Type-Driven Design_ (TyDD), then _Test-Driven Development_ (TeDD),
doing so constrains my code so much, I can only write a correct implementation,
then it comes to the actual implementation (at value-level), that's when TeDD comes into play.

Especially because our code can be wrong, let's replace `any` by `all`:

```haskell
subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = all (flip subscriptionMatches s) ms

subscriptionMatches :: TopicMatcher -> Subscription -> Bool
subscriptionMatches m s = all (topicMatches m) s.topics
```

And our tests are still passing, let's fix this:

We can fix the matchers first:

```haskell
-- (["/a/*", "/a/*/e"], ["/a/c/e"], True)
subscriptionsMatches :: [TopicMatcher] -> Subscription -> Bool
subscriptionsMatches ms s = any (flip subscriptionMatches s) ms
```

And finally the subscriptions:

```haskell
-- (["/a/*"], ["/b", "/a/c"], True)
subscriptionMatches :: TopicMatcher -> Subscription -> Bool
subscriptionMatches m s = any (topicMatches m) s.topics
```

Note: it's the kind of bug mutation testing aims to detect
