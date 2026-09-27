#!/bin/sh
# Builds the fcl-image test runner into tests/build; run from packages/fcl-image.
rm -rf tests/build
mkdir -p tests/build
fpc -Mobjfpc -Sh -Criot -gl -gh -B -vew \
  -Fusrc -Futests -FUtests/build -FEtests/build "$@" tests/testfpimage.lpr
