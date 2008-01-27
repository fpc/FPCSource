#!/bin/sh

suffixes="_all _shared _smart _debug _release"
/bin/ls -1 */Makefile.fpc | while read d; do
  packages=`grep "^packages" $d | cut -f 2 -d '='`
  currpackage=`dirname $d`
  hasdeps=0
  for suf in $suffixes; do
     dep=""
     for p in $packages; do
       dep="${dep}${dep:+ }$p$suf"
     done
     if [ -n "$dep" ]; then
       echo "$currpackage$suf: $dep"
       hasdeps=1
     fi
  done
  [ $hasdeps -eq 1 ] && echo ""
  done
