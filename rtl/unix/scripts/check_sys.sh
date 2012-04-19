#!/usr/bin/env bash

# Script to test fpc to system syscall numbers

# Location of syscall header in system

syscall_header=/usr/include/syscall.h
fpc_sysnr=./sysnr.inc

os=`uname -s`

if [ "$os" == "OpenBSD" ] ; then
  c_syscall_header=sys/syscall.h
else
  c_syscall_header=syscall.h
fi

if ! [ -f $fpc_sysnr ] ; then
  cpu=`fpc -iTP`
  fpc_sysnr=./$cpu/sysnr.inc
fi

verbose=0

os=`uname -s`

# Test C file to grab all loaded headers
cat > test-syscall.c <<EOF
#include <${c_syscall_header}>

int
main ()
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
$CC --save-temps -o test-syscall ./test-syscall.c
res=$?
if [ $res -ne 0 ] ; then
  echo "Call to $CC failed"
  exit
fi
# list of errno.h headers listed
syscall_headers=` sed -n "s:.*\"\(.*/.*\.h\)\".*:\1:p" test-syscall.i |sort | uniq`
echo "Headers found are \"$syscall_headers\""

if [ "$syscall_headers" != "" ] ; then
  syscall_header="$syscall_headers"
fi

fpc_syscall_prefix=syscall_nr_
if [ "$os" == "Linux" ] ; then
  # On Linux system, system call number are defined indirectly
  # with #define SYS_XXX __NR_XXX
  # We look directly for the __NT_ version
  syscall_prefix=__NR_
else
  syscall_prefix=SYS_
fi

# You should only need to change the variables above

sed -n "s:^[ \t]*${fpc_syscall_prefix}\\([_a-zA-Z0-9]*\\)[ \t]*=[ \t]*\\([0-9]*\\).*:check_syscall_number ${syscall_prefix}\1 \2:p" ${fpc_sysnr} > check_sys_list.sh
sed -n "s:^.*define[[:space:]]*${syscall_prefix}\\([_a-zA-Z0-9]*\\)[[:space:]]*\\([0-9]*\\).*:check_syscall_number_reverse ${fpc_syscall_prefix}\1 \2:p" ${syscall_header} > check_sys_list_reverse.sh
 

function check_syscall_number ()
{
  sys=$1
  value=$2
  obsolete=0
  if [[ "$value" =~ ^[0-9]+$ ]] ; then
    eval $sys=\$$value
    if [ $verbose -ne 0 ] ; then
      echo "$sys is $value"
    fi
  else
    eval $sys=$value
    if [ $verbose -ne 0 ] ; then
      echo "$sys set to \"${$sys}\" trough \"$value\""
    fi
  fi
  # Remember this value for later
  eval $sys=$value
  if [ $verbose -ne 0 ] ; then
    echo Testing $sys value $value
  fi
  found=`sed -n "/#define[[:space:]]*${sys}[^A-Za-z0-9_]/p" ${syscall_header}`
  val=`sed -n "s:#define[[:space:]]*${sys}[^A-Za-z0-9_][^A-Za-z0-9_]*\([0-9]*\).*:\1:p" ${syscall_header}`
  if [ $verbose -ne 0 ] ; then
    echo Test for $sys found \"${found}\" \"${value}\" \"${val}\"
  fi
  if [ "${val}" == "${value}" ] ; then
    if [ $verbose -ne 0 ] ; then
      echo ${sys} value ${val} is correct
    fi
  else
    if [ "${val}" == "" ] ; then
      found=`sed -n "/#define.*[^A-Za-z0-9_]${value}$/p" ${syscall_header}`
      if [ "${found}" == "" ] ; then
        found=`sed -n "s:\/\* ${value} is compa:/* ${value} is compa:p" ${syscall_header}`
        if [ "$found" != "" ] ; then
          obsolete=1
        fi
      fi
    fi
    if [ "$found" == "" ] ; then
      found=`grep -n -w $value ${syscall_header}`
    fi
    if [ $obsolete -eq 1 ] ; then
      echo Warning: ${sys} expected ${value}, is obsolete line is \"${found}\"
    else
      echo Problem: ${sys} expected ${value}, line is \"${found}\", val found is \"${val}\"
    fi
  fi
}

function check_syscall_number_reverse ()
{
  sys=$1
  value=$2
  if [ $verbose -ne 0 ] ; then
    echo Testing syscall header entry $sys value $value
  fi
  found=`sed -n "/.*${sys}/p" ${fpc_sysnr}`
  val=`sed -n "s:.*${sys}[ \t]*=[ \t]*\([0-9]*\).*:\1:p" ${fpc_sysnr}`
  if [ $verbose -ne 0 ] ; then
    echo Test for $sys found \"${found}\" \"${value}\" \"${val}\"
  fi
  if [ "${val}" == "${value}" ] ; then
    if [ $verbose -ne 0 ] ; then
      echo ${sys} value ${val} is correct
    fi
  else
    if [ "${val}" == "" ] ; then
      found=`sed -n "/#define.*[^A-Za-z0-9_]${value}$/p" ${syscall_header}`
      if [ "${found}" == "" ] ; then
        found=`sed -n "s:\/\* ${value} is compa: ${value} is compa:p" ${syscall_header}`
      fi
    fi
    echo Problem: ${sys} expected ${value}, line is \"${found}\", val found is \"${val}\"
  fi
}


# Sustitution made to pass from fpc syscall number
# to system define 
set -f

echo "Checking values from \"${fpc_sysnr}\" in \"${syscall_header}\""
source ./check_sys_list.sh
echo "Checking if values in \"${syscall_header}\" are missing in \"${fpc_sysnr}\""
source ./check_sys_list_reverse.sh

