#!/bin/bash
# Build the Google Drive demo with fpc.
cd "$(dirname "$0")" || exit 1
mkdir -p lib
fpc -B -vewn -FUlib -Fu../../src drivedemo.lpr
