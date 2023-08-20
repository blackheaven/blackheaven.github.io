+++
title = "Homelab: server certificates"
date = 2023-08-20
draft = false
path = "2023-08/homelab-init"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

[In previous log](@/2023-08-16_homelab-init.md) I have introduced my homelab infrastructure.

One of the thing I'm the most bothered with, is the lack of ciphering between my desktop computer (_Looping_) and my server (_Barracuda_).

The easiest thing is to generate and expose a server certificate (which will be used to "authenticate" the server and cipher exchanges).

In order to do that we should first create a certificate authority (CA):

```bash
##!/usr/bin/env bash
set -xeuo pipefail

CA_DIR=server/ca
REQ_DIR=server/requests

mkdir -p $CA_DIR
touch $CA_DIR/ca.index
openssl rand -hex 16 > $CA_DIR/ca.serial

echo "./$REQ_DIR/ca.conf.dhall \"$CA_DIR\"" | dhall text > "$CA_DIR/ca.conf"

openssl genpkey -algorithm RSA -aes-256-cbc -out "$CA_DIR/ca.key" -pkeyopt rsa_keygen_bits:4096
chmod 400 "$CA_DIR/ca.key"

openssl req -new -out "$CA_DIR/ca.crt" -config "$CA_DIR/ca.conf" -x509 -days 365 -key "$CA_DIR/ca.key"

openssl x509 -in "$CA_DIR/ca.crt" -out "$CA_DIR/ca.pem" -outform PEM

echo "Copy '$CA_DIR/ca.pem' to localhost@/etc/nixos/certificates/barracuda/ca.pem"
echo "To fit with configuration.nix"
echo "pki.certificateFiles = [ ./certificates/barracuda/ca.pem ];"
sudo cp "$CA_DIR/ca.pem" /etc/nixos/certificates/barracuda/ca.pem
```

Note: in order to generate the configuration files for `openssl` (I mean, [`libressl`(https://www.libressl.org/)]),
I used [`dhall-openssl`](https://github.com/jvanbruegge/dhall-openssl).
It's a genius project which abstract the tedious and mysterious world of OpenSSL configuration files you end up copy-pasting
from the internet (even if you do it professionally for years).

Which gives:

```dhall
let openssl =
      https://raw.githubusercontent.com/jvanbruegge/dhall-openssl/master/package.dhall

in  \(caDir : Text) ->
      openssl.mkCaConfig
        openssl.CaConfig::{
        , distinguishedName = openssl.DistinguishedName::{
          , commonName = "Barracuda Root Certificate Authority"
          }
        , allowedHosts = [ "barracuda.local" ]
        , caDir
        }
```

Simple, straight-to-the-point, genius.

Then, we have to create and sign server certificates:

```bash
#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 1 ];
then
  echo "Usage: $0 SERVICE"
  exit 2
fi

SERVICE=$1
CRT_DIR=server/services/$SERVICE
CA_DIR=server/ca
REQ_DIR=server/requests

mkdir -p "$CRT_DIR"

export TEMP_FILE=$(mktemp)
echo "./$REQ_DIR/server.conf.dhall \"$SERVICE\"" | dhall text > "$TEMP_FILE"

# Create a new certificate
openssl req -new -nodes -newkey rsa:4096 -keyout "$CRT_DIR/server.key" -out "$CRT_DIR/server.csr" -config "$TEMP_FILE"

# Sign it with our CA
# TODO add -days 1
openssl ca -config "$CA_DIR/ca.conf" -cert "$CA_DIR/ca.crt" -keyfile "$CA_DIR/ca.key" -out "$CRT_DIR/server.crt" -infiles "$CRT_DIR/server.csr"

TARGET_DIR=/etc/nixos/certificates/servers/$SERVICE
TARGET_LOGIN=black@192.168.0.4
echo "Copy '$CRT_DIR/server.key' and '$CRT_DIR/server.crt' to barracuda@$TARGET_DIR"
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
scp $CRT_DIR/server.{key,crt} $TARGET_LOGIN:~
ssh $TARGET_LOGIN sudo chown root:root server.key server.crt
ssh $TARGET_LOGIN sudo mv server.key server.crt $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service
```

and the configuration file:

```dhall
let openssl =
      https://raw.githubusercontent.com/jvanbruegge/dhall-openssl/master/package.dhall

in  \(service : Text) ->
      openssl.mkConfig
        openssl.Config::{
        , distinguishedName = openssl.DistinguishedName::{
          , commonName = "${service}.barracuda.local"
          }
        , altNames = [] : List Text
        }
```

Then we have to update _Barracuda_ (nginx), configuration:

```nix
"withings.barracuda.local" = {
  enableACME = false;
  serverAliases = [ ];
  forceSSL = true;
  sslCertificate = "/etc/nixos/certificates/servers/withings/server.crt";
  sslCertificateKey = "/etc/nixos/certificates/servers/withings/server.key";
  locations."/" = {
    proxyPass = "http://127.0.0.1:5555/";
  };
};
```

we should have everything working, except, we have to explicitly disable certificate authority check.
In order to do that, we should modify _Looping_ (and all clients' configuration) to and the CA to the list of trusted CAs.

```nix
security.pki.certificateFiles = [ ./certificates/barracuda/ca.pem ];
```
