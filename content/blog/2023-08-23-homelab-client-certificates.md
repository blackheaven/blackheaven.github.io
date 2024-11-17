+++
title = "Homelab: client certificates"
date = 2023-08-23
draft = false
path = "2023-08/homelab-client-certificates"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

[In previous log](@/blog/2023-08-20-homelab-server-certificates.md) I have set up a server certificate,
which is a way to secure (make it "impossible", well, difficult in fact, to read exchanged data, and authenticate the server).

While it's interesting, it's actually pointless since my services are accessible without authentications
(preventing data being intercepted, while being freely accessible any time is a vain effort).

That's one of the reason of the existence of client certificates: restrict access to authenticated/authorized clients.

Let's start with the beginning, we have to create a certificate authority (I have chosen one per service):

```bash
##!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 1 ];
then
  echo "Usage: $0 SERVICE"
  exit 2
fi

SERVICE=$1
CA_DIR=client/$SERVICE/ca
REQ_DIR=client/requests

mkdir -p "$CA_DIR"
touch "$CA_DIR/ca.index"
openssl rand -hex 16 > "$CA_DIR/ca.serial"

echo "./$REQ_DIR/ca.conf.dhall \"$CA_DIR\" \"$SERVICE\"" | dhall text > "$CA_DIR/ca.conf"

openssl genpkey -algorithm RSA -aes-256-cbc -out "$CA_DIR/ca.key" -pkeyopt rsa_keygen_bits:4096
chmod 400 "$CA_DIR/ca.key"

openssl req -new -out "$CA_DIR/ca.crt" -config "$CA_DIR/ca.conf" -x509 -days 365 -key "$CA_DIR/ca.key"

openssl x509 -in "$CA_DIR/ca.crt" -out "$CA_DIR/ca.pem" -outform PEM

CRL_DIR=$CA_DIR/crl
mkdir -p "$CRL_DIR"
touch "$CRL_DIR/index.txt"
echo 00 > "$CRL_DIR/number"
openssl ca -gencrl -keyfile "$CA_DIR/ca.key" -cert "$CA_DIR/ca.crt" -out $CRL_DIR/crl.pem -config "$CA_DIR/ca.conf"

TARGET_DIR=/etc/nixos/certificates/clients/$SERVICE
TARGET_LOGIN=black@192.168.0.4
echo "Copy '$CA_DIR/ca.crt' and '$CRL_DIR/crl.pem' to barracuda@$TARGET_DIR"
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
scp "$CA_DIR/ca.crt" $TARGET_LOGIN:~
scp "$CRL_DIR/crl.pem" $TARGET_LOGIN:~
ssh $TARGET_LOGIN sudo chown root:root ca.crt crl.pem
ssh $TARGET_LOGIN sudo mv ca.crt crl.pem $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service
```

With the associated configuration file:

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

The main difference with servers' CA are the `crl*` fields which stands for [Certificate Revocation List](https://en.wikipedia.org/wiki/Certificate_revocation_list),
more on this later.

Then, we have to create and sign client certificates:

```bash
#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ];
then
  echo "Usage: $0 SERVICE USER"
  exit 2
fi

SERVICE=$1
USER=$2
CRT_DIR=client/$SERVICE/$USER
CA_DIR=client/$SERVICE/ca
REQ_DIR=client/requests

mkdir -p "$CRT_DIR"

export TEMP_FILE=$(mktemp)
echo "./$REQ_DIR/client.conf.dhall \"$SERVICE\" \"$USER\"" | dhall text > "$TEMP_FILE"

# Create a new certificate
openssl req -new -nodes -newkey rsa:4096 -keyout "$CRT_DIR/client.key" -out "$CRT_DIR/client.csr" -config "$TEMP_FILE"

# Sign it with our CA
openssl ca -config "$CA_DIR/ca.conf" -cert "$CA_DIR/ca.crt" -keyfile "$CA_DIR/ca.key" -out "$CRT_DIR/client.crt" -infiles "$CRT_DIR/client.csr"

# Export client key
openssl pkcs12 -export -clcerts -in "$CRT_DIR/client.crt" -inkey "$CRT_DIR/client.key" -out "$CRT_DIR/client.p12"
openssl pkcs12 -in "$CRT_DIR/client.p12" -out "$CRT_DIR/client.pem" -clcerts

echo "Browser key: $PWD/$CRT_DIR/client.p12"
echo "PEM key: $PWD/$CRT_DIR/client.pem"
```

and the configuration file:

```dhall
let openssl =
      https://raw.githubusercontent.com/jvanbruegge/dhall-openssl/master/package.dhall

in  \(service : Text) ->
    \(user : Text) ->
      openssl.mkConfig
        openssl.Config::{
        , distinguishedName = openssl.DistinguishedName::{
          , commonName = "${user} client certificate for barracuda.${service}"
          }
        }
```

Let's talk about CRL's, unlike for servers, clients' certificates are sprayed into the wild,
so they are more likely to be compromised.

We could, like for the servers, recreate the CA, but doing so would invalidate all accesses to all clients (to their certificates to be precise).
Instead, we can emit a revocation request which will the will be registered on the CRL, then, each time a request come, the given client certificate will be checked against the CRL. 

Let's revoke this:

```bash
#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ];
then
  echo "Usage: $0 SERVICE USER"
  exit 2
fi

SERVICE=$1
USER=$2
CRT_DIR=client/$SERVICE/$USER
CRT_FILE=$CRT_DIR/client.pem
CA_DIR=client/$SERVICE/ca
CRL_DIR=$CA_DIR/crl
NEW_CRL=$CRL_DIR/revoked.crl

openssl ca -config "$CA_DIR/ca.conf" -cert "$CA_DIR/ca.crt" -keyfile "$CA_DIR/ca.key" -revoke "$CRT_FILE"
openssl ca -config "$CA_DIR/ca.conf" -cert "$CA_DIR/ca.crt" -keyfile "$CA_DIR/ca.key" -gencrl -out "$NEW_CRL"

TARGET_DIR=/etc/nixos/certificates/clients/$SERVICE
TARGET_LOGIN=black@192.168.0.4
echo "Copy '$CA_DIR/ca.crt' and '$CRL_DIR/crl.pem' to barracuda@$TARGET_DIR"
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
# scp "$CA_DIR/ca.crt" $TARGET_LOGIN:~
# scp "$CRL_DIR/crl.pem" $TARGET_LOGIN:~
scp "$NEW_CRL" $TARGET_LOGIN:~/crl.pem
# ssh $TARGET_LOGIN sudo chown root:root ca.crt crl.pem
# ssh $TARGET_LOGIN sudo mv ca.crt crl.pem $TARGET_DIR
ssh $TARGET_LOGIN sudo chown root:root crl.pem
ssh $TARGET_LOGIN sudo mv crl.pem $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service
```

Finally we have to update _Barracuda_ (nginx), configuration:

```nix
"withings.barracuda.local" = {
  enableACME = false;
  serverAliases = [ ];
  forceSSL = true;
  sslCertificate = "/etc/nixos/certificates/servers/withings/server.crt";
  sslCertificateKey = "/etc/nixos/certificates/servers/withings/server.key";
  extraConfig = ''
    ssl_client_certificate /etc/nixos/certificates/clients/transmission/ca.crt;
    ssl_crl /etc/nixos/certificates/clients/transmission/crl.pem;
    ssl_verify_client on;
  '';
  locations."/" = {
    proxyPass = "http://127.0.0.1:5555/";
  };
};
```

By now, you should get a HTTP 400 status code on each query you submit.
To finalise the setup, you have to import the `client.p12` in your browser and indicate it to your tools (such as _restic_).
