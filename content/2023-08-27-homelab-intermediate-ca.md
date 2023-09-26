+++
title = "Homelab: Intermediate CA"
date = 2023-08-27
draft = false
path = "2023-08/homelab-intermediate-ca"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

When I have introduced [server certificates](@/2023-08-20-homelab-server-certificates.md) I have used a simple Certificate Authority (CA).
Which means that the trusted CA is also the one signing the server certificate.
Consequently, key and passphrase of the trusted CA should be accessible for server certificate signing phase.

Doing so prevents the deployment on the server (_Barracuda_), which also prevents regular certificates generation (more on that in the next log).

That why we want to rely on an _Intermediate CA_, so we'll end up with:

* a _root CA_, which will be trusted by the clients (i.e. _Looping_)
* an _Intermediate CA_ deployed on _Barracuda_ and signing _server certificates_

Note that I base my set-up on [Jamie Nguyen's OpenSSL Certificate Authority guide](https://jamielinux.com/docs/openssl-certificate-authority/),

First, we have to generate le _root CA_:

```
##!/usr/bin/env bash
set -xeuo pipefail

CA_DIR=server/generated/ca-root
REQ_DIR=server/requests
COMMON_NAME="Barracuda Root Certificate Authority"

mkdir -p $CA_DIR
touch $CA_DIR/ca.index
openssl rand -hex 16 > $CA_DIR/ca.serial

mkdir "$CA_DIR/certs" "$CA_DIR/newcerts" "$CA_DIR/private"

echo "./$REQ_DIR/mk-ca.conf.dhall \"$CA_DIR\" \"$COMMON_NAME\"" \
  | dhall text > "$CA_DIR/ca.conf"

openssl genpkey \
  -algorithm RSA -aes-256-cbc \
  -pkeyopt rsa_keygen_bits:4096 \
  -out "$CA_DIR/private/ca.key.pem"
chmod 400 "$CA_DIR/private/ca.key.pem"

openssl req \
  -new \
  -config "$CA_DIR/ca.conf" \
  -key "$CA_DIR/private/ca.key.pem" \
  -x509 -days 365 \
  -out "$CA_DIR/certs/ca.cert.pem"

openssl x509 \
  -outform PEM \
  -in "$CA_DIR/certs/ca.cert.pem" \
  -out "$CA_DIR/ca.pem"

echo "Copy '$CA_DIR/ca.pem' to localhost@/etc/nixos/certificates/barracuda/ca.pem"
echo "To fit with configuration.nix"
echo "pki.certificateFiles = [ ./certificates/barracuda/ca.pem ];"
sudo cp "$CA_DIR/certs/ca.cert.pem" /etc/nixos/certificates/barracuda/ca.pem

```

Note: file names slightly change, beside of that, the main part happen in `mk-ca.conf.dhall`:

```
let openssl =
      https://raw.githubusercontent.com/jvanbruegge/dhall-openssl/master/package.dhall

let KeyUsage = openssl.KeyUsage

in  \(caDir : Text) ->
    \(commonName : Text) ->
      openssl.mkCaConfig
        openssl.CaConfig::{
        , distinguishedName = openssl.DistinguishedName::{
          , commonName
          , country = Some "FR"
          , state = Some "France"
          , locality = Some "Lyon"
          , organization = Some "Black"
          , organizationalUnit = Some "A-Team"
          }
        , allowedHosts = [ "barracuda.local" ]
        , caDir
        , defaultPolicy = Some "root_ca_policy"
        , privateKey = Some "\$base_dir/private/ca.key.pem"
        , certificate = Some "\$base_dir/certs/ca.cert.pem"
        , usage =
          [ KeyUsage.CrlSign, KeyUsage.KeyCertSign, KeyUsage.DigitalSignature ]
        }

```

Let's focus on two points:

* `defaultPolicy` is now stricter (i.e. it requires all the `DistinguishedName` attributes to be filled and match them during chain certificates check)
* `usage`is increased with `DigitalSignature`

Then we have to generate and sign _Intermediate CA_:

```
##!/usr/bin/env bash
set -xeuo pipefail

CA_ROOT_DIR=server/generated/ca-root
CA_PARENT_DIR=server/generated
CA_DIRNAME=ca-intermediate
CA_DIR=$CA_PARENT_DIR/$CA_DIRNAME
REQ_DIR=server/requests
COMMON_NAME="Barracuda Intermediate CA ($(date --utc +"%Y-%m-%dT%H:%M:%SZ"))"

mkdir -p $CA_DIR
touch $CA_DIR/ca.index
openssl rand -hex 16 > $CA_DIR/ca.serial

mkdir "$CA_DIR/certs" "$CA_DIR/newcerts" "$CA_DIR/private"

echo "./$REQ_DIR/mk-ca.conf.dhall \"$CA_DIR\" \"$COMMON_NAME\"" \
  | dhall text > "$CA_DIR/ca.conf"

openssl genpkey \
  -algorithm RSA -aes-256-cbc \
  -pkeyopt rsa_keygen_bits:4096 \
  -out "$CA_DIR/private/ca.key.pem"
chmod 400 "$CA_DIR/private/ca.key.pem"

openssl req \
  -new \
  -key "$CA_DIR/private/ca.key.pem" \
  -config "$CA_DIR/ca.conf" \
  -out "$CA_DIR/certs/ca.csr.pem"

echo "./$REQ_DIR/sign-intermediate-ca.conf.dhall \"$CA_ROOT_DIR\" \"$COMMON_NAME\"" \
  | dhall text > "$CA_DIR/sign-ca.conf"

openssl ca \
  -batch \
  -config "$CA_DIR/sign-ca.conf" \
  -notext \
  -in "$CA_DIR/certs/ca.csr.pem" \
  -out "$CA_DIR/certs/ca.cert.pem"


openssl x509 \
  -outform PEM \
  -in "$CA_DIR/certs/ca.cert.pem" \
  -out "$CA_DIR/ca.pem"

TARGET_DIR=/etc/nixos/certificates/$CA_PARENT_DIR
TARGET_LOGIN=black@192.168.0.4
BACKUPS_DIR=/etc/nixos/certificates/backups
echo "Copy '$CA_DIR' to barracuda@$TARGET_DIR"
scp -r $CA_DIR $TARGET_LOGIN:~
ssh $TARGET_LOGIN sudo chown -R root:root $CA_DIRNAME
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
ssh $TARGET_LOGIN sudo mv "$TARGET_DIR/$CA_DIRNAME" "$BACKUPS_DIR/server_$(date --utc +"%Y-%m-%dT%H:%M:%SZ")" || true
ssh $TARGET_LOGIN sudo mv $CA_DIRNAME $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service

FINAL_CHAIN_FILE=/etc/nixos/certificates/barracuda/ca.pem
TMP_CHAIN_FILE=$(mktemp /tmp/certs-chain-file.XXXXXX)
cat "$CA_DIR/certs/ca.cert.pem" "$CA_ROOT_DIR/certs/ca.cert.pem" > "$TMP_CHAIN_FILE"
sudo cp "$TMP_CHAIN_FILE" "$FINAL_CHAIN_FILE"

```

Let's break this down:

* We create a new CA (with a different `commonName` to avoid the check failing due to an attack suspicion)
* We sign it with the _Root CA_
* We build a _Certificates Chain File_, so we can check server certificates against it

Note: here is `sign-intermediate-ca.conf.dhall`:

```
let openssl =
      https://raw.githubusercontent.com/jvanbruegge/dhall-openssl/master/package.dhall

let KeyUsage = openssl.KeyUsage

in  \(caDir : Text) ->
    \(commonName : Text) ->
      openssl.mkCaConfig
        openssl.CaConfig::{
        , distinguishedName = openssl.DistinguishedName::{ commonName }
        , allowedHosts = [ "barracuda.local" ]
        , caDir
        , defaultPolicy = Some "intermediate_ca_policy"
        , defaultDays = 32
        , privateKey = Some "\$base_dir/private/ca.key.pem"
        , certificate = Some "\$base_dir/certs/ca.cert.pem"
        , pathlen = Some 0
        , usage =
          [ KeyUsage.CrlSign, KeyUsage.KeyCertSign, KeyUsage.DigitalSignature ]
        }
```

And that's it!

Generating server certificates don't change (except that it now take _Intermediate CA_ instead of the _Root CA_).

Next time we'll see how and why regenerating short-lived server certificates.
