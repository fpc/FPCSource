#!/bin/bash
PACKAGESDIR=..
TEMPLATEDIR=./
initial_conversion=false

while getopts ":d:p:i" opt; do
  case $opt in
    i)
      initial_conversion=true
      ;;
    d)
      PACKAGESDIR=$OPTARG
      ;;
    p)
      TEMPLATEDIR=$OPTARG/
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

if $initial_conversion
then
  FPMAKEOPT="-i -d $PACKAGESDIR"
else
  FPMAKEOPT="-d $PACKAGESDIR"
fi

eval $(find */Makefile.fpc -printf 'if [ %h = "paszlib" -o %h = "fcl-process" -o %h = "libtar" -o %h = "hash" ] ; then "$TEMPLATEDIR"convert_fpmake.sh -T "$TEMPLATEDIR"Makefile.fpmake.bs.template $FPMAKEOPT %h ; elif [ ! %h = "fpmkunit" ] ; then "$TEMPLATEDIR"convert_fpmake.sh -T "$TEMPLATEDIR"Makefile.fpmake.template $FPMAKEOPT %h ; fi ;' )

