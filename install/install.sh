#!/bin/sh
#
# Free Pascal installation script for Linux.
# Copyright 1996-2000 Michael Van Canneyt and Peter Vreman
#
# Don't edit this file. 
# Everything can be set when the script is run.
#

# Release Version
VERSION=1.0.4

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
LIBDIR=$PREFIX/lib/fpc/$VERSION
SRCDIR=$PREFIX/src/fpc-$VERSION
DOCDIR=$PREFIX/doc/fpc-$VERSION
DEMODIR=$DOCDIR/examples
EXECDIR=$PREFIX/bin

# Install compiler/RTL. Mandatory.
echo Unpacking ...
tar xf binary.tar
echo Installing compiler and RTL ...
unztar baselinux.tar.gz $PREFIX
rm -f $EXECDIR/ppc386
ln -sf $LIBDIR/ppc386 $EXECDIR/ppc386
echo Installing utilities...
unztar utillinux.tar.gz $PREFIX
if yesno "Install FCL"; then
    unztar unitsfcllinux.tar.gz $PREFIX
fi
if yesno "Install API"; then
    unztar unitsapilinux.tar.gz $PREFIX
fi
if yesno "Install Base (zlib,ncurses,x11) Packages"; then
    unztar unitsbaselinux.tar.gz $PREFIX
fi
if yesno "Install Net (inet,uncgi) Packages"; then
    unztar unitsnetlinux.tar.gz $PREFIX
fi
if yesno "Install Database (mysql,interbase,postgres) Packages"; then
    unztar unitsdblinux.tar.gz $PREFIX
fi
if yesno "Install Graphics (svgalib,opengl,ggi,forms) Packages"; then
    unztar unitsgfxlinux.tar.gz $PREFIX
fi
if yesno "Install Misc (utmp,paszlib) Packages"; then
    unztar unitsmisclinux.tar.gz $PREFIX
fi
rm -f *linux.tar.gz
echo Done.
echo

# Install the sources. Optional.
if yesno "Install sources"; then
  echo Unpacking ...
  tar xf sources.tar
  echo Installing sources in $SRCDIR ...
  unztar basesrc.tar.gz $PREFIX
  if yesno "Install compiler source"; then
    unztar compilersrc.tar.gz $PREFIX
  fi    
  if yesno "Install RTL source"; then
    unztar rtlsrc.tar.gz $PREFIX
  fi    
  if yesno "Install FCL source"; then
    unztar fclsrc.tar.gz $PREFIX
  fi    
  if yesno "Install API source"; then
    unztar apisrc.tar.gz $PREFIX
  fi    
  if yesno "Install Packages source"; then
    unztar packagessrc.tar.gz $PREFIX
  fi    
  if yesno "Install Utils source"; then
    unztar utilsrc.tar.gz $PREFIX
  fi    
  rm -f *src.tar.gz
  echo Done.
fi
echo

# Install the documentation. Optional.
if yesno "Install documentation"; then
  echo Installing documentation in $DOCDIR ...
  unztar docs.tar.gz $PREFIX
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

# Install /etc/fpc.cfg, this is done using the samplecfg script
$LIBDIR/samplecfg $LIBDIR

# The End
echo
echo End of installation. 
echo
echo Refer to the documentation for more information.
echo
