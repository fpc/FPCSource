#!/usr/bin/env bash
#
# Free Pascal installation script for Unixy platforms.
# Copyright 1996-2004 Michael Van Canneyt, Marco van de Voort and Peter Vreman
#
# Don't edit this file. 
# Everything can be set when the script is run.
#

# Release Version %version% will be replaced by makepack
VERSION=%version%

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

# Untar tar.gz file ($2) from file ($1) and untar result to the given directory ($3)
unztarfromtar ()
{
 tar -xOf $HERE/$1 $2 | tar --directory $3 -xzf -
}

# Get file list from tar archive ($1) in variable ($2)
# optionally filter result through sed ($3)
listtarfiles ()
{
  askvar=$2
  if [ ! -z $3 ]; then
    list=`tar tvf $1 | awk '{ print $(NF) }' | sed -n /$3/p`
  else
     list=`tar tvf $1 | awk '{ print $(NF) }'`
  fi
  eval $askvar='$list'
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

# Install files from binary-*.tar
#  $1 = cpu-target
#  $2 = cross prefix
installbinary ()
{
  if [ "$2" == "" ]; then
    FPCTARGET=$1
    CROSSPREFIX=
  else
    FPCTARGET=`echo $2 | sed 's/-$//'`
    CROSSPREFIX=$2
  fi  
  
  BINARYTAR=${CROSSPREFIX}binary.$1.tar
  
  # conversion from long to short archname for ppc<x>
  case $FPCTARGET in
    m68k*)
      PPCSUFFIX=68k;;
    sparc*) 
      PPCSUFFIX=sparc;;
    i386*)
      PPCSUFFIX=386;;
    powerpc*)
      PPCSUFFIX=ppc;;
    arm*)
      PPCSUFFIX=arm;;
    x86_64*)
      PPCSUFFIX=x64;;
  esac

  # Install compiler/RTL. Mandatory.
  echo "Installing compiler and RTL for $FPCTARGET..."
  unztarfromtar $BINARYTAR ${CROSSPREFIX}base.$1.tar.gz $PREFIX
  
  # Install symlink
  rm -f $EXECDIR/ppc${PPCSUFFIX}
  ln -sf $LIBDIR/ppc${PPCSUFFIX} $EXECDIR/ppc${PPCSUFFIX}
  
  echo "Installing utilities..."
  unztarfromtar $BINARYTAR ${CROSSPREFIX}utils.$1.tar.gz $PREFIX
  if yesno "Install Textmode IDE"; then
      unztarfromtar $BINARYTAR ${CROSSPREFIX}ide.$1.tar.gz $PREFIX
  fi
  if yesno "Install FCL"; then
      unztarfromtar $BINARYTAR ${CROSSPREFIX}units-fcl.$1.tar.gz $PREFIX
  fi
  if yesno "Install packages"; then
    listtarfiles $BINARYTAR packages units
    for f in $packages 
    do
      if [ $f != ${CROSSPREFIX}units-fcl.$1.tar.gz ]; then
        p=`echo "$f" | sed -e 's+^.*units-\([^\.]*\)\..*+\1+'`
	echo "Installing $p"
        unztarfromtar $BINARYTAR $f $PREFIX
      fi
    done
  fi
  rm -f *.$1.tar.gz
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
# If we can't write on prefix, select subdir of home dir
if [ ! -w $PREFIX ]; then
  PREFIX=$HOME/fpc-$VERSION
fi
ask "Install prefix (/usr or /usr/local) " PREFIX
makedirhierarch $PREFIX

# Set some defaults.
LIBDIR=$PREFIX/lib/fpc/$VERSION
SRCDIR=$PREFIX/src/fpc-$VERSION
EXECDIR=$PREFIX/bin
OSNAME=`uname -s | tr A-Z a-z`

BSDHIER=0
case $OSNAME in 
*bsd)
  BSDHIER=1;;
esac

SHORTARCH=$ARCHNAME

FULLARCH=$ARCHNAME-$OSNAME
DOCDIR=$PREFIX/share/doc/fpc-$VERSION
DEMODIR=$DOCDIR/examples

# Install all binary releases
for f in *binary*.tar
do
  target=`echo $f | sed 's+^.*binary\.\(.*\)\.tar$+\1+'`
  cross=`echo $f | sed 's+binary\..*\.tar$++'`
  installbinary $target $cross
done

echo Done.
echo

# Install the documentation. Optional.
if yesno "Install documentation"; then
  echo Installing documentation in $DOCDIR ...
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

# Install /etc/fpc.cfg, this is done using the samplecfg script
$LIBDIR/samplecfg $LIBDIR

# The End
echo
echo End of installation. 
echo
echo Refer to the documentation for more information.
echo
