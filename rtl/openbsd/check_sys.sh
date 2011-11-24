#!/usr/bin/env bash

# Script to test fpc to system syscall numbers

# Location of syscall header in system

syscall_header=/usr/include/sys/syscall.h

verbose=0

# Sustitution made to pass from fpc syscall number
# to system define 
fpc_syscall_prefix=syscall_nr_
syscall_prefix=SYS_

# You should only need to change the variables above

sed -n "s:^[ \t]*${fpc_syscall_prefix}\\([_a-zA-Z0-9]*\\)[ \t]*=[ \t]*\\([0-9]*\\).*:check_syscall_number ${syscall_prefix}\1 \2:p" sysnr.inc > check_sys_list.sh

function check_syscall_number ()
{
  sys=$1
  value=$2
  if [ $verbose -ne 0 ] ; then
    echo Testing $sys value $value
  fi
  found=`sed -n "/#define.*${sys}[^A-Za-z0-9_]/p" ${syscall_header}`
  val=`sed -n "s:#define.*${sys}[^A-Za-z0-9_][^A-Za-z0-9_]*\([0-9]*\).*:\1:p" ${syscall_header}`
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
    echo problem for ${sys} expected ${value}, line is \"${found}\", val found is \"${val}\"
  fi
}

source ./check_sys_list.sh

