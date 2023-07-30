#!/bin/bash
#
# Check directory
#
if [ -z "$1" ]; then
  PACKAGEDIR=$(pwd)
else
  PACKAGEDIR=$1
  if [ ! -d $PACKAGEDIR ]; then
    echo "The directory $PACKAGEDIR does not exist"
    exit 1
  fi  
fi 
#
# Check package dir ?
#
if [ ! -f $PACKAGEDIR/build/Makefile.pkg ]; then
  echo "This script must be executed in the rtl directory or have an argument to specify the package directory"
  exit 1
fi
#
# fpcmake to use
#
if [ -e $PACKAGEDIR/../utils/fpcm/fpcmake ]; then
  FPCMAKE=$PACKAGEDIR/../utils/fpcm/fpcmake
else
  FPCMAKE=fpcmake
fi  
#
# Go
#
echo "Using fpcmake: $FPCMAKE"
#
# Main
#
cd $PACKAGEDIR
echo "Doing package toplevel dir: $PACKAGEDIR"
$FPCMAKE -q -Tall
cd $PACKAGEDIR
echo "Recreating Makefile in directory $PACKAGEDIR/build"
$FPCMAKE -Tall -q -o Makefile.pkg $PACKAGEDIR/build/Makefile.fpc
sed -i '/PACKAGE_NAME=fcl/ d' $PACKAGEDIR/build/Makefile.pkg
echo "Recreating Makefile in directory $PACKAGEDIR/fpmkunit"
$FPCMAKE -Tall -q $PACKAGEDIR/fpmkunit/Makefile.fpc
