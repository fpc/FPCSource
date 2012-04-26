#!/usr/bin/env bash

# Small script to test fpc to system error numbers

addtoerrno=0
errnonew=./errno-new.inc

temps="check_errno_list.sh check_reverse_errno_list.sh test-errno* $errnonew"

if [ "$1" == "clean" ] ; then
  echo "Deleting $temps"
  rm -f $temps
  exit
fi

if [ "$1" == "verbose" ] ; then
  verbose=1
  echo "Being verbose"
  shift
else
  verbose=0
fi

# Location of error number in system header

errno_header="/usr/include/asm-generic/errno-base.h /usr/include/asm-generic/errno.h"
errno_include=./errno.inc

# Sustitution made to pass from fpc syscall number
# to system define 
fpc_errno_prefix=ESysE
errno_prefix=E

# Test C file to grab all loaded headers
cat > test-errno.c <<EOF
#include <errno.h>

int
dummy ()
{
  return 0;
}
EOF

# Default C compiler is gcc
# Can be overwritten by setting CC variable
# But I don't know if other compilers also generate
# .i files with --save-temps option
if [ "$CC" == "" ] ; then
  CC=gcc
fi

# Use gcc with --save-temps option to create .i file
$CC --save-temps -c ./test-errno.c
# list of errno.h headers listed
errno_headers=` sed -n "s:.*\"\(.*\.h\)\".*:\1:p" test-errno.i |sort | uniq`
echo "Headers found are \"$errno_headers\""

if [ "$errno_headers" != "" ] ; then
  errno_header="$errno_headers"
fi


# You should only need to change the variables above

sed -n "s:^[[:space:]]*${fpc_errno_prefix}\\([_a-zA-Z0-9]*\\)[[:space:]]*=[[:space:]]*\\([0-9][0-9]*\\).*:check_errno_number ${errno_prefix}\1 \2:p" ${errno_include} > check_errno_list.sh
sed -n "s:#define[[:space:]]*${errno_prefix}\\([_a-zA-Z0-9]*\\)[[:space:]][[:space:]]*\\(-*[0-9A-Za-z_]*\\).*:check_reverse_errno_number ${fpc_errno_prefix}\1 \2:p" ${errno_header} > check_reverse_errno_list.sh

function check_errno_number ()
{
  sys=$1
  value=$2
  if [ $verbose -ne 0 ] ; then
    echo Testing $sys value $value
  fi
  # Remember value of this constant
  eval ${sys}=${value}

  found=`sed -n "/#define[[:space:]][[:space:]]*${sys}[^A-Za-z0-9_]/p" ${errno_header}`
  val=`sed -n "s:#define[[:space:]][[:space:]]*${sys}[^A-Za-z0-9_][^A-Za-z0-9_]*\([0-9]*\).*:\1:p" ${errno_header}`
  extval=`sed -n "s:#define[[:space:]][[:space:]]*${sys}[^A-Za-z0-9_][^A-Za-z0-9_]*\([0-9A-Za-z_]*\).*:\1:p" ${errno_header}`
  if [ $verbose -ne 0 ] ; then
    echo Test for $sys found \"${found}\" \"${value}\" \"${val}\"
  fi
  if [ "${val}" == "${value}" ] ; then
    if [ $verbose -ne 0 ] ; then
      echo ${sys} value ${val} is correct
    fi
  else
    if [ "${val}" == "" ] ; then
      foundvalue=`sed -n "/#define.*[^A-Za-z0-9_]${value}$/p" ${errno_header}`
      if [ "${foundvalue}" == "" ] ; then
        foundvalue=`sed -n "s:\/\* ${value} is compa: ${value} is compa:p" ${errno_header}`
      fi
    fi
    if [ "$extval" != "" ] ; then
      eval indirectval=\$$extval
      echo "indirectval =\"$indirectval\" for \"$extval\""
      if [ "$indirectval" != "$value" ] ; then
        echo problem for ${sys} expected ${value}, line is \"${found}\", val found is \"${indirectval}\"
      else
        echo "$sys is defined as $extval which is $indirectval as expected $value"
      fi
    else
      echo "problem for ${sys} expected ${value}, line is \"${found}\", val found is \"${val}\""
    fi
  fi
}


function check_reverse_errno_number ()
{
  errname=$1
  errvalue=$2
  found=`grep -i -w $1 ${errno_include}`
  if [ "${found}" == "" ] ; then
    echo "Error ${errname}, value ${errvalue}, not in ${errno_include} file"
    if [ $addtoerrno -eq 0 ] ; then
      addtoerrno=1
      echo "{ List of missing system error number found in $errno_header }" > $errnonew
    fi
    echo "  $errname = $errvalue;" >> $errnonew
  fi
}

set -f
echo "Checking all entries of ${errno_include} file"
source ./check_errno_list.sh

echo "Checking all defines from \"$errno_header\""
source ./check_reverse_errno_list.sh

if [ $addtoerrno -eq 1 ] ; then
  echo " Missing error number found"
  echo " New values are in \"$errnonew\"
  echo You might want to add these lines to \"$errno_include\""
fi

