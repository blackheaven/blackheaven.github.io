+++
title = "Access Control: topaz"
date = 2024-01-10
draft = false
path = "2024-01/access-control-topaz"

[taxonomies]
categories = ["dev"]
tags = ["access control", "security", "draft concepts"]
+++

In the last log, we have studied [Open Policy Agent](@/2024-01-07_access-control-opa.md)
which introduced Policy-as-code, sadly, we cannot easily represent [ReBAC](@/2023-12-27_access-control-rebac.md)
with it.

That's where [topaz](https://www.topaz.sh/) kicks-in.

Note: at the time of writing, topaz is a bit older than one-years-old,
has less than 300 commits done by less than 20 people, I won't recommend to
use it in production (I don't recommend not running it in production, but if I
have the decision to take, it won't be my go-to choice).

Let's draft a simple file access policy:

```rego
allowed {
  ds.check_permission({
    "object": {
      "key": input.resource.file,
      "type": "file"
    },
    "permission": {"name": "read"},
    "subject": {
      "key": input.user.key,
      "type": "user"
    }
  })
}
```

Note: topaz use OPA's policy language

We can define our objects:

 ```json
{
  "objects": [
    {
      "type": "user",
      "id": "alice",
      "properties": {
        "verified": true
      }
    },
    {
      "type": "user",
      "id": "bob",
      "properties": {
        "verified": true
      }
    },
    {
      "type": "file",
      "id": "/etc/alice/diary.txt",
      "properties": {
        "sensitive": true
      }
    }
  ]
}
```

Then the relations:

 ```json
{
  "relations": [
    {
      "object_type": "user",
      "object_id": "alice",
      "relation": "owns",
      "subject_type": "file",
      "subject_id": "/etc/alice/diary.txt"
    }
  ]
}
```

Finally we can query them with the inputs:

 ```json
{
  "subject": {
    "type": "user",
    "key": "bob"
  },
  "relation": {
    "name": "owns",
    "objectType": "file"
  },
  "object": {
    "type": "file",
    "key": "/etc/alice/diary.txt"
  }
}
```
