+++
title = "Homelab: Regenerating certificates"
date = 2023-08-30
draft = false
path = "2023-08/homelab-certificate-regeneration"

[taxonomies]
categories = ["ops"]
tags = ["ops", "nix", "nixos", "pki"]
+++

Previously, we spent huge amount of time to [set up an intermediate CA](@/blog/2023-08-27-homelab-intermediate-ca.md).

Our goal was to be able to deploy it on _Barracuda_, so it can automatically generate trusted certificates.
The idea, borrowed from [Meta engineering blog](https://engineering.fb.com/2023/08/07/security/short-lived-certificates-protect-tls-secrets/), is to ensure that, even if a certificate is broken, it won't affect for a long time.

Let's start by changing the way we're generating certificated, to work in a non-interactive mode:

```bash
#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ];
then
  echo "Usage: $0 PASSFILE SERVICE"
  exit 2
fi

PASS_FILE=$1
SERVICE=$2
CRT_DIR=server/generated/services/$SERVICE
CA_DIR=server/generated/ca-intermediate
REQ_DIR=server/requests
COMMON_NAME="$SERVICE.barracuda.local ($(date --utc +"%Y-%m-%dT%H:%M:%SZ"))"

mkdir -p "$CRT_DIR"

echo "./$REQ_DIR/server.conf.dhall \"$COMMON_NAME\"" \
  | dhall text > "$CRT_DIR/certificate-request.con"

# Create a new certificate
openssl req \
  -new \
  -config "$CRT_DIR/certificate-request.con" \
  -nodes -newkey rsa:4096 \
  -keyout "$CRT_DIR/server.key" \
  -out "$CRT_DIR/server.csr"

# Sign it with our CA
openssl ca \
  -batch -passin file:$PASS_FILE \
  -config "$CA_DIR/ca.conf" \
  -cert "$CA_DIR/certs/ca.cert.pem" \
  -keyfile "$CA_DIR/private/ca.key.pem" \
  -days 1 \
  -out "$CRT_DIR/server.crt" \
  -infiles "$CRT_DIR/server.csr"

TARGET_DIR=/etc/nixos/certificates/servers/$SERVICE
TARGET_LOGIN=black@192.168.0.4
echo "Copy '$CRT_DIR/server.key' and '$CRT_DIR/server.crt' to barracuda@$TARGET_DIR"
ssh $TARGET_LOGIN sudo mkdir -p $TARGET_DIR
scp $CRT_DIR/server.{key,crt} $TARGET_LOGIN:~
ssh $TARGET_LOGIN sudo chown root:root server.key server.crt
ssh $TARGET_LOGIN sudo mv server.key server.crt $TARGET_DIR
ssh $TARGET_LOGIN sudo systemctl restart nginx.service
```

Then, we have to set up a cron job (well, a systemd timer):

```nix
systemd.timers."certs-regen-withing" = {
  wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1m";
      OnCalendar = "*-*-* 3:00:00";
      Unit = "certs-regen-withing.service";
    };
};

systemd.services."certs-regen-withing" = {
  script = ''
    set -eu
    /etc/nixos/generateServerServiceBatch.sh "/etc/nixos/certs-pw" "withing"
    ${pkgs.systemd}/bin/systemctl restart nginx.service
  '';
  serviceConfig = {
    Type = "oneshot";
    User = "root";
    WorkingDirectory=/etc/nixos
  };
};
```

Finally, the certificate will be regenerated at 3 AM every day (when I'm "usually" sleeping).
