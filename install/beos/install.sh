#!/bin/sh
#
# Free Pascal installation script for BeOS.
# Copyright 1996-2002 Michael Van Canneyt, Peter Vreman
# and Carl Eric Codere
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

# noyes gives 0 on no, 1 on yes $1 gives text to display.
noyes ()
{
  while true; do
  echo -n "$1 (Y/n) ? "
  read ans
  case X$ans in
   X|Xy|XY) return 1;;
   Xn|XN) return 0;;
  esac
  done
}


# Untar files ($3,optional) from  file ($1) to the given directory ($2)
unztar ()
{
 unzip $HERE/$1 -d $2
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

# Verifying minimal operating system version
echo Verifying operating system version.
OS_VER=`uname -r`
echo Operating system version : $OS_VER
echo Minimal operating system version required : 4.5
if noyes "Continue installation?"; then
    exit
fi

# Install in /fpc or /fpc-$VERSION ?
if checkpath /fpc-$VERSION; then
    PREFIX=/fpc
else
    PREFIX=/fpc
fi
ask "Install prefix (/fpc or /fpc-$VERSION) " PREFIX
makedirhierarch $PREFIX

# Set some defaults.
LIBDIR=$PREFIX/units
SRCDIR=$PREFIX/src
DOCDIR=$PREFIX/doc/
DEMODIR=$PREFIX/examples
EXECDIR=$PREFIX/bin

# Install compiler/RTL. Mandatory.
echo Unzipping ...
unzip binary.zip
echo Installing compiler and RTL ...
unztar basebe.zip $PREFIX
echo Installing utilities...
unztar utilbe.zip $PREFIX
if yesno "Install FCL"; then
    unztar ufclbe.zip $PREFIX
fi
if yesno "Install paszlib Package"; then
    unztar upaszlibbe.zip $PREFIX
fi
if yesno "Install regexpr Package"; then
    unztar uregexprbe.zip $PREFIX
fi
rm -f *be.zip
echo Done.
echo

# Install the sources. Optional.
if yesno "Install sources"; then
  echo Unpacking ...
  unzip sources.zip
  echo Installing sources in $SRCDIR ...
  unztar basesrc.zip $PREFIX
  if yesno "Install compiler source"; then
    unztar fpcompilersrc.zip $PREFIX
  fi    
  if yesno "Install RTL source"; then
    unztar rtlsrc.zip $PREFIX
  fi    
  if yesno "Install FCL source"; then
    unztar fclsrc.zip $PREFIX
  fi    
  if yesno "Install paszlib source"; then
    unztar paszlibsrc.zip $PREFIX
  fi    
  if yesno "Install regexpr source"; then
    unztar regexprsrc.zip $PREFIX
  fi   
  if yesno "Install Utils source"; then
    unztar fputilsrc.zip $PREFIX
  fi    
  rm -f *src.zip
  echo Done.
fi
echo

# Install the documentation. Optional.
if yesno "Install documentation"; then
  echo Installing documentation in $DOCDIR ...
  unztar doc-html.zip $PREFIX
  echo Done.
fi
echo

# Install the demos. Optional.
if yesno "Install demos"; then
  ask "Install demos in" DEMODIR
  echo Installing demos in $DEMODIR ...
  makedirhierarch $DEMODIR
  unztar demo.zip $DEMODIR
  echo Done.
fi
echo

# Install /etc/fpc.cfg, this is done using the samplecfg script
echo Installing sample configuration file
./samplecfg $PREFIX $HOME

if yesno "Extend PATH to include installed compiler binary"; then
  echo Changing PATH ...
  echo PATH='$PATH:'$PREFIX/bin/beos >> $HOME/.profile
  echo Restart your shell so the changes take effect.
fi

# The End
echo
echo End of installation. 
echo 
echo Read the documentation for further information.
echo
