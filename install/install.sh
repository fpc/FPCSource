#!/bin/sh
#
# Free Pascal installation script for Linux.
# Michael Van Canneyt, 1996-1999
#
# Don't edit this file. 
# Everything can be set when the script is run.
#

# Release Version
VERSION=0.99.12

# some useful functions
# ask displays 1st parameter, and ask new value for variable, whose name is
# in the second parameter.
ask ()
{
askvar=$2
eval old=\$$askvar
eval echo -n \""$1 [$old] : "\" 
read $askvar
eval test -z \"\$$askvar\" && eval $askvar=\'$old\'
}
# yesno gives 1 on no, 0 on yes $1 gives text to display.
yesno ()
{
  while true; do
  echo -n "$1 (Y/N) ? "
  read ans
  case $ans in
   y|Y) return 0;;
   n|N) return 1;;
  esac
  done
}

# Untar files ($3,optional) from  file ($1) to the given directory ($2)
unztar ()
{
 tar -xzf $HERE/$1 --directory $2 $3
}

# Make all the necessary directories to get $1
makedirhierarch ()
{
  OLDDIR=`pwd`
  case $1 in
    /*) cd /;;
  esac
  OLDIFS=$IFS;IFS=/;eval set $1; IFS=$OLDIFS
  for i
  do
    test -d $i || mkdir $i || break
    cd $i ||break
  done
  cd $OLDDIR
}

# check to see if something is in the path
checkpath ()
{
 ARG=$1
 OLDIFS=$IFS; IFS=":";eval set $PATH;IFS=$OLDIFS
 for i
 do
   if [ $i = $ARG ]; then
     return 0
   fi
 done 
 return 1
}

# Here we start the thing.

# Set some defaults.
LIBDIR=/usr/lib/fpc/$VERSION
SRCDIR=/usr/src/fpc-$VERSION
DOCDIR=/usr/doc/fpc-$VERSION
DEMODIR=$DOCDIR/demo

HERE=`pwd`
if checkpath /usr/local/bin; then
   EXECDIR=/usr/local/bin
else
   EXECDIR=/usr/bin
fi

# welcome message.
clear
echo "This shell script will attempt to install the Free Pascal Compiler"
echo "version $VERSION" in the directories of your choice.
echo 

# Install libraries. Mandatory.

ask "Install libraries in" LIBDIR 
echo Installing libraries in $LIBDIR ...
makedirhierarch $LIBDIR
unztar libs.tar.gz $LIBDIR
echo Done.
echo

# Install the program. Mandatory.

ask "Install program in" EXECDIR
echo Installing program in $EXECDIR ...
makedirhierarch $EXECDIR
ln -sf $LIBDIR/ppc386 $EXECDIR/ppc386
echo Done.
echo

# Install the sources. Optional.
if yesno "Install sources"; then
  ask "Install sources in" SRCDIR
  echo Installing sources in $SRCDIR ...
  makedirhierarch $SRCDIR
  unztar sources.tar.gz $SRCDIR
  echo Done.
fi
echo

# Install the documentation. Optional.
if yesno "Install documentation"; then
  ask "Install documentation in" DOCDIR
  echo Installing documentation in $DOCDIR ...
  makedirhierarch $DOCDIR
  unztar docs.tar.gz $DOCDIR
  echo Done.
fi
echo

# Install the demos. Optional.
if yesno "Install demos"; then
  ask "Install demos in" DEMODIR
  echo Installing demos in $DEMODIR ...
  makedirhierarch $DEMODIR
  unztar demo.tar.gz $DEMODIR
  echo Done.
fi
echo

# Install /etc/ppc386.cfg, this is done using the samplecfg script
$LIBDIR/samplecfg $LIBDIR

# The End
echo
echo End of installation. 
echo
echo Refer to the documentation for more information.
echo
