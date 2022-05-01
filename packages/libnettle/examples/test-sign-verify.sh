#!/bin/sh
set -e

echo "Signing rsaverify.lpr, writing signature to sig.txt"
./rsasign testkey rsaverify.lpr sig.txt

echo "validating signature of rsaverify.lpr, reading signature from sig.txt"
./rsaverify testkey.pub rsaverify.lpr sig.txt
