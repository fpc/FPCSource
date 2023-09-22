#!/bin/bash
#
# Check directory
#

if [ "${PATH/cygdrive/}" != "$PATH" ] ; then
  inCygwin=1
else
  inCygwin=0
fi

if [ -z "$1" ]; then
  RTLDIR=$(pwd)
else
  RTLDIR=$1
  if [ ! -d $RTLDIR ]; then
    echo "The directory $RTLDIR does not exist"
    exit 1
  fi
fi
if [ $inCygwin -eq 1 ] ; then
  echo "Cygwin system detected"
  NEWRTLDIR=`cygpath -da $RTLDIR`
  echo "RTLDIR=$NEWRTLDIR"
  if [ -n "$NEWRTLDIR" ] ; then
    RTLDIR="$NEWRTLDIR"
  fi
fi
#
# Check rtl dir ?
#
if [ ! -d "$RTLDIR/ucmaps" ]; then
  echo "This script must be executed in the rtl directory or have an argument tto specify the RTL directory"
  exit 1
fi
#
# fpcmake to use
#
if [ -e "$RTLDIR/../utils/fpcm/fpcmake" ]; then
  FPCMAKE="$RTLDIR/../utils/fpcm/fpcmake"
else
  FPCMAKE=fpcmake
fi
#
# Go
#
echo "Using fpcmake: \"$FPCMAKE\""
#
# Main
#
echo "Doing RTL toplevel dir: \"$RTLDIR\""
pushd "$RTLDIR" >/dev/null 2>&1
$FPCMAKE -q -Tall
popd >/dev/null 2>&1
#
# OS-specific
#
for d in *
do
  if [ -f "$d/Makefile.fpc" ]; then
    echo "Doing directory $d"
    pushd "$RTLDIR/$d" >/dev/null 2>&1
    case $d in
      darwin)
        TARGETS="darwin,ios,iphonesim" ;;
      macos)
        TARGETS="macosclassic" ;;
      *)
        TARGETS="$d" ;;
    esac
    CMD="$FPCMAKE -T$TARGETS -q -x $RTLDIR/inc/Makefile.rtl"
    echo "Command: $CMD"
    $CMD
    popd >/dev/null 2>&1
  fi
done
#
# That's all, folks!
#
 
