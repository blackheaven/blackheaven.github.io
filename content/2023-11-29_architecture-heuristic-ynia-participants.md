+++
title = "Architecture Heuristic YNIA: Participants"
date = 2023-11-29
draft = false
path = "2023-11/architecture-heuristic-ynia-participants"

[taxonomies]
categories = ["Software engineering"]
tags = ["architecture", "design", "heuristics"]
+++

A while ago, I have introduced [_YNIA_](@/2023-10-18_architecture-heuristic-ynai-queues.md),
which is a set of patterns I set up before _really_ need them.

It's not uncommon, when producing software to have to design collaborative spaces.

A good example is the chat.

You can come up with the design of a message like this:

```haskell
data Message = Message
  { author :: UserId
  , creation :: UTCTime
  , body :: Text
  }
```

I think it's pretty minimal.

From my experience, stakeholders tend to be creative with collaborations.

A common use-case is when the user left the tool.

If you use RDBMS, you can get in trouble with your queries if you delete the
user's entry.

You could use a `Maybe UserId`, but you'd have to update all user's messages.
Doing so would also make all messages from different past user look the same.

You may also come up with these requirements:

* Being able to have a name per collaborative space (chat session)
* Being anonymized when you leave the group
* Being able to collaborate without being registered (and associate contributions a posteriori)

That's why I introduce `Participant`:

```haskell
data Message = Message
  { author :: ParticipantId
  , creation :: UTCTime
  , body :: Text
  }
```

Which is linked to:

```haskell
data Participant
  = Registered DisplayName UserId
  | Anonymous DisplayName Token
  | Deleted UUID
```

Which gives me the freedom to have a dedicated lifecycle for `Participant`s.
