+++
title = "Homelab: Monthly certificate regeneration deployment issue"
date = 2023-12-03
draft = false
path = "2023-12/homelab-agenix-nginx-monthly-regen"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

I spend the most important part of the maintenance time of _Barracuda_ (my HomeLab)
regenerating mes certificates ([Server](@/2023-08-20-homelab-server-certificates.md) and [Client](@/2023-08-23-homelab-client-certificates.md)).

While it often takes few commands:

```bash
agebox decrypt --all --force -i @/secrets
for s in irc monitoring restic withings; do scripts/generateClientCRL.sh $s looping; done
agebox encrypt --all
deploy '.#barracuda'
```

([Client certificates only need their CLR to be regenerated](@/2023-10-22-homelab-client-certificates-crl-regeneration.md))

It does not work for nginx.

I can import the client certificates in my browser, frenetically for refresh,
I still get a _400_ invalid certificate.

The issue comes from nginx, which reads certificates at startup, and, since [agenix](https://github.com/ryantm/agenix)
(which manages the secrets) does not change the paths when the file change,
nginx configuration does not change, so nginx is not restarted, consequently
my certificate is checked against the previous version (which expired).

In order to fix it, I force the secret filename to depend on file's content:

```nix
age = {
  secrets = let
    nginx = name: localPath: {
      name = "${name}-${builtins.hashFile "md5" localPath}";
      file = localPath;
      mode = "400";
      owner = "nginx";
      group = "nginx";
    };
  in {
    ircClientCA = nginx "ircClientCA"
      ../certificates/client/generated/irc/ca/ca.pem.agebox;
    ircClientCRL = nginx "ircClientCRL"
      ../certificates/client/generated/irc/ca/crl/crl.pem.agebox;
    ircServerCRT = nginx "ircServerCRT"
      ../certificates/server/generated/services/irc/server.crt.agebox;
    ircServerKEY = nginx "ircServerKEY"
      ../certificates/server/generated/services/irc/server.key.agebox;
  };
}
```

Note: I have used `MD5` hashing to have to "small" filename suffix.

It's not great, and I still have to regenerate them (which takes me few minutes,
but I have to enter multiples passwords, confirm multiple times, etc.).

My guess is that it would be a good use-case for [HashiCorp's Vault](https://www.vaultproject.io/),
but it'll be the topic of a next log.
