#!/bin/sh
#
# Free Pascal installation script for Solaris
# Copyright 1996-2002 Michael Van Canneyt and Peter Vreman
#
# Don't edit this file. 
# Everything can be set when the script is run.
#

# Release Version
VERSION=1.0.6

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
  echo -n "$1 (Y/n) ? "
  read ans
  case X$ans in
   X|Xy|XY) return 0;;
   Xn|XN) return 1;;
  esac
  done
}

# Untar files ($3,optional) from  file ($1) to the given directory ($2)
unztar ()
{
 ./gzip -d $HERE/$1.tar.gz
 ./gtar -xvf $HERE/$1.tar --directory $2 $3
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

# --------------------------------------------------------------------------
# welcome message.
#

clear
echo "This shell script will attempt to install the Free Pascal Compiler"
echo "version $VERSION with the items you select"
echo 

# Here we start the thing.
HERE=`pwd`

# Install in /usr/local or /usr ?
if checkpath /usr/local/bin; then
    PREFIX=/usr/local
else
    PREFIX=/usr
fi
ask "Install prefix (/usr or /usr/local) " PREFIX
makedirhierarch $PREFIX

# Set some defaults.
LIBDIR=$PREFIX/lib/fpc
SRCDIR=$PREFIX/src/fpc
DOCDIR=$PREFIX/doc/fpc
DEMODIR=$DOCDIR/examples
EXECDIR=$PREFIX/bin

# Install compiler/RTL. Mandatory.
echo Unpacking ...
tar xf binary.tar
echo Installing compiler and RTL ...
unztar basesun $PREFIX
rm -f $EXECDIR/ppc386
ln -sf $LIBDIR/$VERSION/ppc386 $EXECDIR/ppc386
echo Installing GNU Assembler and Linker Mandatory.
unztar asldsun $PREFIX
echo Installing utilities...
unztar utilsun $PREFIX
if yesno "Install GNU Tools"; then
    unztar makesun $PREFIX
fi
if yesno "Install FCL"; then
    unztar ufclsun $PREFIX
fi
if yesno "Install Regular expression Package"; then
    unztar uregexprsun $PREFIX
fi
if yesno "Install paszlib Package"; then
    unztar upaszlibsun $PREFIX
fi
rm -f *sun.tar.gz
echo Done.
echo

# Install the sources. Optional.
if yesno "Install sources"; then
  echo Unpacking ...
  tar xf sources.tar
  echo Installing sources in $SRCDIR ...
  unztar basesrc $PREFIX      
  if yesno "Install compiler source"; then
    unztar compilersrc $PREFIX
  fi    
  if yesno "Install RTL source"; then
    unztar rtlsrc $PREFIX
  fi    
  if yesno "Install FCL source"; then
    unztar fclsrc $PREFIX
  fi    
  if yesno "Install paszlib source"; then
    unztar upaszlibsrc $PREFIX
  fi    
  if yesno "Install regexpr source"; then
    unztar uregexprsrc $PREFIX
  fi   
  if yesno "Install Utils source"; then
    unztar utilssrc $PREFIX
  fi
  rm -f *src.tar.gz
  echo Done.
fi
echo

# Install the documentation. Optional.
if yesno "Install documentation"; then
  echo Installing documentation in $DOCDIR ...
  unztar fpcdoc $PREFIX
  echo Done.
fi
echo

# Install the demos. Optional.
if yesno "Install demos"; then
  ask "Install demos in" DEMODIR
  echo Installing demos in $DEMODIR ...
  makedirhierarch $DEMODIR
  unztar demo $DEMODIR
  echo Done.
fi
echo

# Install /etc/fpc.cfg, this is done using the samplecfg script
$LIBDIR/$VERSION/samplecfg $LIBDIR

# The End
echo
echo End of installation. 
echo
echo Refer to the documentation for more information.
echo
