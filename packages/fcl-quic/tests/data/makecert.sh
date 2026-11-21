#!/bin/sh
# Generates the self-signed test certificate (CN and SAN localhost) and its P-256 key.
cd "$(dirname "$0")" || exit 1
openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes -days 36500 \
  -subj "/CN=localhost" -addext "subjectAltName=DNS:localhost,IP:127.0.0.1" \
  -keyout server.key -out server.crt
