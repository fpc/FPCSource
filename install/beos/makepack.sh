#!/bin/sh
#
# Shell script to make a FPC .tar package for BeOS systems
# Copyright 1996-2002 Michael Van Canneyt and Peter Vreman
#

# Version
VERSION=1.0.6
RELEASE=ELF

unset FPCDIR

# Goto the toplevel if necessary
[ -d install ] || cd ..

make beoszip RELEASE=1
make sourcezip USEZIP=1
make docsrc USEZIP=1
make docs USEZIP=1
make demozip USEZIP=1

SOURCES=`/bin/ls *src.zip`
FILES=`/bin/ls *be.zip demo.zip`
RELFILES="binary.zip sources.zip demo.zip docs.zip install.sh"

echo Creating binary.zip
zip binary.zip $FILES
echo Creating sources.zip
zip sources.zip $SOURCES
echo Copying install.sh
cp install/beos/install.sh .
chmod 755 install.sh

echo Creating fpc-$VERSION-$RELEASE.zip
zip fpc-$VERSION-$RELEASE.zip $RELFILES
