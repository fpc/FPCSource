#!/bin/bash
# Build the Gmail demo with fpc.
cd "$(dirname "$0")" || exit 1
mkdir -p lib
fpc -B -vewn -FUlib -Fu../../src gmaildemo.lpr
