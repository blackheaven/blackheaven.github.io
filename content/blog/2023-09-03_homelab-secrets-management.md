+++
title = "Homelab: Secrets management"
date = 2023-09-03
draft = false
path = "2023-09/homelab-secrets-management"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "secrets-management"]
+++

Since the introduction of [the certificates](@/blog/2023-08-20-homelab-server-certificates.md),
each script was ending with some old-fashioned (i.e. `ssh`/`scp`-based commands) as follows:

```bash
TARGET_DIR=/etc/nixos/certificates/servers/$SERVICE
TARGET_LOGIN=black@192.168.0.4
echo "Copy '$CRT_DIR/server.key' and '$CRT_DIR/server.crt' to barracuda@$TARGET_DIR"
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
scp $CRT_DIR/server.{key,crt} $TARGET_LOGIN:~
ssh $TARGET_LOGIN sudo chown root:root server.key server.crt
ssh $TARGET_LOGIN sudo mv server.key server.crt $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service
```

I won't say that old things are always bad, but there are many shortcomings:

* My server's IP is hard-coded
* My server file hierarchy is implicitly coupled
* My script handles (poorly) nginx
* My script is stateful and does not handle well failures (no retry/restart/rollback)

All of these making it hard to maintain/reuse.

If I really wanted to make this clean from here, I would use something like
[ansible](https://www.ansible.com/) (I mean [JetPorch](https://www.jetporch.com/)),
or [Terraform](https://www.terraform.io/), which I'm more use to.

I could simply reference the files in my `configuration.nix` file, but it would
end-up in my git history, which is not a big deal as it is a local git repository,
the real concern come from my `nix store`, which will store the certificate and make
them available to everyone on my machine, in clear-text, without permission requirements.

Hopefully, there's a solution called [agenix](https://github.com/ryantm/agenix) for that.

It relies on [age](https://github.com/FiloSottile/age), which is a [pgp](https://datatracker.ietf.org/doc/html/rfc4880)
successor, which aims to provide a simple way to cipher/decipher files.

Lastly, there's also [agebox](https://github.com/slok/agebox) which helps to manage
multiple ciphered files/directories.

Let's start by creating a key:

```bash
age-keygen -o secrets/age00.txt
```

Then I save the public key:

```bash
echo age... > keys/age00.pub.txt
```

I could generate it from an SSH key, but I'm used to having my SSH keys
dedicated to SSH usage.

Then, I use `agebox` to convert all my secrets (certificates and keys):

```bash
agebox init
agebox encrypt server/generated/services/*/server.{crt,key} client/generated/*/ca/{ca.pem,crl/crl.pem}
```

Which suffixes all the files with `.agebox`.

Then I upload `keys` to _Barracuda_.

Finally, the nix part.

First, we have to declare all files:

```nix
age = {
  identityPaths = [ "/etc/nixos/secrets/age00.txt" ];
  secrets =
    let
      nginx = localPath: { file = localPath; mode = "400"; owner = "nginx"; group = "nginx"; };
    in
    {
      resticClientCA = nginx ./certificates/client/generated/restic/ca/ca.pem.agebox;
      resticClientCRL = nginx ./certificates/client/generated/restic/ca/crl/crl.pem.agebox;
      withingsClientCA = nginx ./certificates/client/generated/withings/ca/ca.pem.agebox;
      withingsClientCRL = nginx ./certificates/client/generated/withings/ca/crl/crl.pem.agebox;
    };
};
```

Quick note here: by default everything is done to use SSH keys, I have to
explicitly set the private part of the age key.

Lastly, we can reference the declared secrets:

```nix
virtualHosts = {
  "restic.barracuda.local" = {
    # ...
    extraConfig = ''
      ssl_client_certificate ${config.age.secrets.withingsClientCA.path};
      ssl_crl ${config.age.secrets.withingsClientCRL.path};
      ssl_verify_client on;
    '';
  };
};
```

Here we are, now, our scripts aren't tied to _Barracuda_, IP, file hierarchy or
deployment failure mishandling.
There's more, aside of `/etc/nixos/secrets`, we don't even need a specific file
organisation, which means less headaches for me.
