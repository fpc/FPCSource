#!/bin/bash
cd ..
make clean all OPT=-gl
if [ $? != 0 ]; then
  exit
fi
cd tests
fpc dbtestframework.pas -glh -Fu../units/x86_64-linux/
if [ $? != 0 ]; then
  exit
fi
if [ "$1" != "" ]; then
  ./dbtestframework --suite=$1
else
  ./dbtestframework
fi  
#
#
  
