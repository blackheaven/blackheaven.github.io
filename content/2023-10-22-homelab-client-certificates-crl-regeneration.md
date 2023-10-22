+++
title = "Homelab: client certificates CRL regeneration"
date = 2023-10-22
draft = false
path = "2023-10/homelab-client-certificates-crl-regeneration"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

Two months ago, I have set up [authentication through client certificates](@/2023-08-23-homelab-client-certificates.md).

For reference, I have used this configuration:

```dhall
let openssl =
      https://raw.githubusercontent.com/blackheaven/dhall-openssl/master/package.dhall

in  \(caDir : Text) ->
    \(service : Text) ->
      openssl.mkCaConfig
        openssl.CaConfig::{
        , distinguishedName = openssl.DistinguishedName::{
          , commonName = "${service} client Root Certificate Authority"
          }
        , allowedHosts = [ "barracuda.local" ]
        , caDir
        , crlDir = Some "\$base_dir/crl"
        , crl = Some "\$base_dir/crl.pem"
        , crlNumber = Some "\$base_dir/number"
        , defaultCrlDays = Some 30
        }
```

Last month, my certificates were rejected, I have blindly regenerated my CAs.

In fact, it was due to the expiration of the [Certificate Revocation List](https://en.wikipedia.org/wiki/Certificate_revocation_list).

I have tried to regenerate the CRL certificate:

```bash
##!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 1 ];
then
  echo "Usage: $0 SERVICE"
  exit 2
fi

SERVICE=$1
CA_DIR=client/generated/$SERVICE/ca
CRL_DIR=$CA_DIR/crl
openssl ca \
  -gencrl \
  -config "$CA_DIR/ca.conf" \
  -cert "$CA_DIR/ca.crt" \
  -keyfile "$CA_DIR/ca.key" \
  -out $CRL_DIR/crl.pem
```

I have re-deployed it, but nothing changed.

The thing is `nginx` has to be reloaded in order to take it in account.
