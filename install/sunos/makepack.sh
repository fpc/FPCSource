#!/bin/sh
#
# Shell script to make a FPC .tar package for Solaris systems
# Copyright 1996-2002 Michael Van Canneyt and Peter Vreman
#

# Version
VERSION=1.0.6
RELEASE=ELF

unset FPCDIR

# Goto the toplevel if necessary
[ -d install ] || cd ..

#make sunoszip
#make sourcezip
#make docsrc
#make docs
#make demozip

SOURCES=`/bin/ls *src.tar.gz`
FILES=`/bin/ls *sun.tar.gz *exm.tar.gz`
RELFILES="binary.tar sources.tar demo.tar.gz docs.tar.gz install.sh gtar gzip"

echo Creating binary.tar
tar cf binary.tar $FILES
echo Creating sources.tar
tar cf sources.tar $SOURCES
echo Copying install.sh
cp install/sunos/install.sh .
chmod 755 install.sh

echo Creating fpc-$VERSION.$RELEASE.tar
tar cf fpc-$VERSION.$RELEASE.tar $RELFILES
