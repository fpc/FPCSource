#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling $1
/usr/local/bin/sparc-linux/as -o "$1.o" "$1.s"
if [ $? != 0 ]; then DoExitAsm system; fi
echo Assembling system
/usr/local/bin/sparc-linux/as -o system.o system.s
/usr/local/bin/sparc-linux/as -o prt0.o ../../rtl/linux/sparc/prt0.as
if [ $? != 0 ]; then DoExitAsm sparctest; fi
echo Linking sparctest
/usr/local/bin/sparc-linux/ld    -s -L. -o "$1" link.res
if [ $? != 0 ]; then DoExitLink sparctest; fi
