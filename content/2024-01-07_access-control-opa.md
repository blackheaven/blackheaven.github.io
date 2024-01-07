+++
title = "Access Control: Open Policy Agent"
date = 2024-01-07
draft = false
path = "2024-01/access-control-opa"

[taxonomies]
categories = ["dev"]
tags = ["access control", "security", "draft concepts"]
+++

After introducing [DAC](@/2023-12-31_access-control-dac-mac.md), we have proposed
[capabilities](2024-01-03_access-control-capabilities.md).

Our implementation was really simple and purely in-memory.

Hopefully there are implementations which can hold rules and data such as
[Open Policy Agent (OPA)](https://www.openpolicyagent.org/).

OPA allows to decouple application code and policy expression/enforcement.
So, we are able to deploy, review and tests both part independently.

OPA is able to implement many access control schemes.

OPA can be run as a CLI, a server (with a REST API), a library.

To run, OPA needs a policy, data and inputs.

Let's say we want (again) to represent an UNIX-like file access control.

We have our data:

```json
{
    "files": [
        {
            "path": "/home/alice/afile.txt",
            "owner": "alice",
            "group": "users",
            "permissions": {
                "owner": ["read", "write", "execute"],
                "group": ["read"],
                "others": []
            }
        },
        {
            "path": "/home/bob/afile.txt",
            "owner": "bob",
            "group": "users",
            "permissions": {
                "owner": ["read", "write"],
                "group": [],
                "others": []
            }
        }
    ],
    "users": [
        {
            "name": "alice",
            "groups": ["users"]
        },
        {
            "name": "bob",
            "groups": ["users"]
        },
        {
            "name": "charlie",
            "groups": ["root"]
        }
    ]
}
```

Then our rules:

```rego
package rules

import rego.v1

default allow := false

access_request := input.access_request

# Owner
allow if {
	some file in data.files
	file.path == access_request.path
	file.owner == access_request.user
	some allowed_action in file.permissions.owner
	allowed_action == access_request.action
}

# Group
allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	some group in user.groups
	file.group == group
	some allowed_action in file.permissions.group
	allowed_action == access_request.action
}

# Others
allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	every group in user.groups {
		file.group != group
	}
	some allowed_action in file.permissions.others
	allowed_action == access_request.action
}

# Root
allow if {
	some file in data.files
	file.path == access_request.path
	some user in data.users
	user.name == access_request.user
	some group in user.groups
	group == "root"
}
```

We can vary our inputs:

```json
{
    "access_request": {
        "path": "/home/bob/afile.txt",
        "user": "charlie",
        "action": "read"
    }
}
```

Which gives this kind of outputs:

```json
{
    "access_request": {
        "action": "read",
        "path": "/home/bob/afile.txt",
        "user": "charlie"
    },
    "allow": true
}
```
