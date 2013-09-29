#!/bin/bash
TEMPLATEFILE=Makefile.fpmake.template
PACKAGESDIR=..
initial_conversion=false

while getopts ":T:d:i" opt; do
  case $opt in
    T)
      TEMPLATEFILE=$OPTARG
      ;;
    i)
      initial_conversion=true
      ;;
    d)
      PACKAGESDIR=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG"
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." 
      exit 1
      ;;
  esac
done

shift $((OPTIND-1))

if [ ! "$#" -eq 1 ] 
then
  echo "Usage: convert_fpmake.sh [-T templatename] [-d packagesdir] [packagename]"
  exit
fi
if [ ! -d $1 ] 
then
  echo "Package $1 not found"
  exit
fi
if [ ! -e $TEMPLATEFILE ] 
then
  echo "Package template $TEMPLATEFILE not found"
  exit
fi

echo "Processing package $1"
if $initial_conversion
then
  if [ -f $1/Makefile.fpc.fpcmake ]
  then
    echo "Package is al geconverteerd"
    exit
  fi

  rm -f $1/Makefile.fpc.fpcmake
  svn rename $1/Makefile.fpc $1/Makefile.fpc.fpcmake
fi

cp -v $TEMPLATEFILE $1/Makefile.fpc
sed -i s/name=insert-name-here/name=$1/ $1/Makefile.fpc
sed -i s,insert-packageunits-here,$PACKAGESDIR, $1/Makefile.fpc

if $initial_conversion
then
  svn add $1/Makefile.fpc
fi

cd $1
fpcmake -TAll
cd ..
