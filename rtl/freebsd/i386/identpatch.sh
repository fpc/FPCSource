#!/bin/sh
elfdump -n `which elfdump` |awk '/FreeBSD/{print $2}' >elfversion
IDVERSION=`cat elfversion`
rm elfversion
echo Patching cprt0.as with version $IDVERSION

sed -I.sav -es/504000/$IDVERSION/ cprt0.as