#!/bin/sh

# List of files which should be considered imprtant
# for the behavior of fpcmake binary
# fpcmake.inc and Makefile should be excluded from that list
if [ "$#" -ge 1 ] ; then
important_files="$*"
else
important_sources=`ls -1 fpcm*.pp fpcmake.ini Makefile.fpc`
fi

LANG=C
export LANG

tmpfiles=

for f in $important_sources ; do
  tmpfile=.tmp.$f
  tmpfiles="$tmpfiles $tmpfile"
  svn info $f > $tmpfile
done

# echo "svn_info is $svn_info"
svn_date=`gawk '/Last Changed Date: / {print $4 }' $tmpfiles | sort -n | tail -1`
svn_rev=`gawk '/Last Changed Rev: / {print $4 }' $tmpfiles | sort -n | tail -1`
echo "for files $important_sources, date is $svn_date, rev is $svn_rev"
echo "'$svn_date rev $svn_rev'" > revision.inc
rm -Rf $tmpfiles



