#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling system
/usr/local/bin/sparc-linux/as -o system.o system.s
if [ $? != 0 ]; then DoExitAsm system; fi
echo Assembling sparctest
/usr/local/bin/sparc-linux/as -o test0001.o test0001.s
if [ $? != 0 ]; then DoExitAsm sparctest; fi
echo Linking sparctest
/usr/local/bin/sparc-linux/ld    -s -L. -o test0001 link.res
if [ $? != 0 ]; then DoExitLink sparctest; fi
