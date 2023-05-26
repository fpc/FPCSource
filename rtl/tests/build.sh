#!/bin/bash
if [ -z $1 ]; then
  SRC=testexrtti11.pp
else
  SRC=$1
  shift
fi     
CMD="/Users/michael/Projects/fpc/compiler/ppca64 -vwhn -Fu../units/aarch64-darwin  $SRC $*"
echo "Command: $CMD"
$CMD

