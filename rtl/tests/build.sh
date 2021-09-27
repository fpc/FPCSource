#!/bin/bash
if [ -z $1 ]; then
  SRC=testextrtti.pp
else
  SRC=$1
  shift
fi     
exec /home/michael/fpc/compiler/ppcx64 -vwhn -Fu../units/x86_64-linux -gl $SRC $*
