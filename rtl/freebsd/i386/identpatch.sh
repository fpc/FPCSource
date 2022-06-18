#!/bin/sh
if [ "$#" -ne 1 ]
then
elfdump -n `which elfdump` |awk '/FreeBSD/{print $2}'| head -n1    >elfversion
IDVERSION=`cat elfversion`
rm elfversion
else
IDVERSION=$1
fi
echo Patching cprt0.as with version $IDVERSION

sed -I.sav -es/1201000/$IDVERSION/ cprt0.as
sed -I.sav -es/1201000/$IDVERSION/ dllprt0.as
sed -I.sav -es/1201000/$IDVERSION/ prt0.as
sed -I.sav -es/1201000/$IDVERSION/ gprt0.as
sed -I.sav -es/1201000/$IDVERSION/ si_c.inc
sed -I.sav -es/1201000/$IDVERSION/ si_prc.inc

