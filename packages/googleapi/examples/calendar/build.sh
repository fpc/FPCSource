#!/bin/bash
# Build the Google Calendar demo with fpc.
cd "$(dirname "$0")" || exit 1
mkdir -p lib
fpc -B -vewn -FUlib -Fu../../src calendardemo.lpr
