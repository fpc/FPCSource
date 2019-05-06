#!/usr/bin/env bash

# Script to test fpc to system syscall numbers

# Location of syscall header in system

syscall_header=/usr/include/syscall.h
fpc_sysnr=./sysnr.inc

i=0
for arg in $* ; do
  let i++
  echo "Handling arg $i, \"$arg\""
  if [ "${arg//=}" != "$arg" ] ; then
    echo "Evaluating $arg"
    eval $arg
  elif [ "$arg" == "-v" ] ; then
    verbose=1
  else
    echo "arg not handled!"
  fi
done

start_pwd=`pwd`
start_dir=`basename $start_pwd`

if [ -d "rtl" ] ; then
  echo "Entering rtl directory"
  cd rtl
fi

os=`uname -s | tr [:upper:] [:lower:] `
os_cpu=`uname -m | tr [:upper:] [:lower:] `
now_pwd=`pwd`
now_dir=`basename $now_pwd`
if [ -d "$os" ] ; then
  echo "Entering $os directory"
  cd $os
fi

case "$os" in
  freebsd|openbsd|netbsd) c_syscall_header=sys/syscall.h;;
  *)  c_syscall_header=syscall.h;;
esac

if [ -z "$FPC" ] ; then
  FPC=fpc
fi

if [ ! -f $fpc_sysnr ] ; then
  cpu=`$FPC -iTP`
  if [ "${cpu//sparc/}" != "$cpu" ] ; then
    cpu=sparcgen
  fi
  fpc_sysnr=./$cpu/sysnr.inc
  if [ ! -f "$fpc_sysnr" ] ; then
    fpc_sysnr=`ls -1 ./${cpu}*/sysnr.inc| head -1`
  fi
  if [ ! -f "$fpc_sysnr" ] ; then
    if [ "${cpu//sparc/}" != "$cpu" ] ; then
      cpu=sparcgen
    fi
    fpc_sysnr=./$cpu/sysnr.inc
    if [ ! -f "$fpc_sysnr" ] ; then
      echo "sysnr.inc file not found, try again in rtl/$os directory"
      exit
    fi
  fi
fi

if [ -f "$fpc_sysnr" ] ; then
  echo "Checking $fpc_sysnr content for Free Pascal syscall numbers"
  fpc_sysnr_dir=`dirname $fpc_sysnr `
  sysnr_includes=`grep -o '{\$i  *[a-z_A-Z0-9/.-]*' $fpc_sysnr | sed 's:.*{\$i *:'$fpc_sysnr_dir/: `
  if [ -n "$sysnr_includes" ] ; then
    echo "Found $sysnr_includes include files"
    fpc_sysnr="$fpc_sysnr $sysnr_includes"
  fi
fi

if [ -z "$verbose" ] ; then
  verbose=0
fi

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
if [ -z "$CC" ] ; then
  CC=gcc
fi

cpu=`$FPC -iTP`
cpu_source=`$FPC -iSP`
is_16=0
is_32=0
is_64=0
case $cpu in
  aarch64)   is_64=1;;
  alpha)     is_32=1;;
  arm)       is_32=1;;
  avr)       is_16=1;;
  i386)      is_32=1;;
  i8086)     is_16=1;;
  ia64)      is_64=1;;
  jvm)       is_32=1;;
  m68k)      is_32=1;;
  mips)      is_32=1;;
  mipsel)    is_32=1;;
  powerpc)   is_32=1;;
  powerpc64) is_64=1;;
  riscv32)   is_32=1;;
  riscv64)   is_64=1;;
  sparc)     is_32=1;;
  sparc64)   is_64=1;;
  vis)       is_32=1;;
  x86_64)    is_64=1;;
esac

if [ $is_64 -eq 1 ] ; then
  if [ "$os_cpu" == "aarch64" ] ; then
    CC_OPT="$CC_OPT -Wall"
  else
    CC_OPT="$CC_OPT -m64 -Wall"
  fi
  CPUBITS=64
elif [ $is_32 -eq 1 ] ;then
  if [ "$os_cpu" == "aarch64" ] ; then
    CC=arm-linux-gnueabihf-gcc-4.8
    export BINUTILSPREFIX=arm-linux-
  fi
  if [ "${FPC/ppcarm/}" != "$FPC" ] ; then
    CC_OPT="$CC_OPT -march=armv7-a -Wall"
  elif [ "${os_cpu/arm/}" != "$os_cpu" ] ; then
    CC_OPT="$CC_OPT -march=armv6 -Wall"
  else
    CC_OPT="$CC_OPT -m32 -Wall"
  fi

  CPUBITS=32
elif [ $is_16 -eq 1 ] ; then
  CPUBITS=16
else
  CPUBITS=unknown
fi

# Use gcc with --save-temps option to create .i file
echo "Calling $CC $CC_OPT --save-temps -o test-syscall ./test-syscall.c"
$CC $CC_OPT --save-temps -o ./test-syscall ./test-syscall.c
res=$?
if [ $res -ne 0 ] ; then
  echo "Call to $CC failed"
  exit
else
  rm -f ./test-syscall.c ./test-syscall
fi
# list of errno.h headers listed
syscall_headers=` sed -n "s:.*\"\(.*/.*\.h\)\".*:\1:p" test-syscall.i |sort | uniq`
rm -f test-syscall.*
echo "C syscall headers files found are \"$syscall_headers\""

if [ -n "$syscall_headers" ] ; then
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

awkfile=preproc.awk
tmp_fpc_sysnr=tmp-sysnr-${cpu}.inc

c_syscall_source=test-syscall-${cpu}.c

# Test C file to grab all loaded headers
# must be called with -DSYS_MACRO=$sys
cat > $c_syscall_source <<EOF
#include <${c_syscall_header}>
#include <stdio.h>

int
main ()
{
  printf ("%d\n", (int) SYS_MACRO);
  return 0;
}
EOF

cat > $awkfile <<EOF
BEGIN {IGNORECASE = 1;
enable=1;
macro="";
incfile="";
cpu= "cpu" proc;
cpubits= "cpu" cpubits;
}
/\{\\\$i / { incfile=\$2;
  print "Include file  " incfile " found"; }
/\{\\\$ifdef / { macro=gensub("[^A-Za-z_0-9].*","",1,\$2);
  if ( (macro == cpu) || (macro == cpubits)) { enable=1;
    print "// ifdef " macro " found and accepted at line " FNR;
  } else {enable=0;
    print "// ifdef " macro " found and rejected at line " FNR;
  };
  }
/\{\\\$ifndef / { macro=gensub("[^A-Za-z_0-9].*","",1,\$2);
  if ( (macro == cpu) || (macro == cpubits) ) { enable=0;
   print "// ifndef " macro " found and rejected at line " FNR;
 } else {enable=1;
   print "// ifndef " macro " found and accepted at line " FNR;
 };
  }
/\{\\\$else/ { if (enable == 1) {enable=0;} else {enable = 1;}}
/.*/ { if (enable == 1) {
  wholeline=\$0;
  code=gensub("{.*}","","g",\$0);
  code=gensub("[(][*].*[*][)]","","g",code);
  # Special code to substitute = $HexaDecimal by = 0xHexaDEcimal
  code=gensub("= *\\$","= 0x","g",code);
  comments=gensub(code,"",1,\$0);
  comments1=gensub(".*({.*}).*","\1","g",comments);
  if (comments == comments1)
    comments1="";
  comments2=gensub(".*[(][*].*[*][)]).*","\1","g",comments);
  if (comments == comments2)
    comments2="";
  comments3=gensub(".*//","",1,comments);
  if (comments == comments3)
    comments3="";
  all_comments= comments1 comments2 comments3;
  if (all_comments != "")
    print code "// " comments1 comments2 comments3 ;
  else
    print code;
  } 
}
/\{\\\$endif/ {enable=1;}
EOF

if [ -z "$AWK" ] ; then
  AWK=`which gawk 2> /dev/null`
fi

if [ -z "$AWK" ] ; then
  AWK=`which awk 2> /dev/null`
fi

if [ -n "$AWK" ] ; then
  echo "Preprocessing ${fpc_sysnr} to $tmp_fpc_sysnr"
  echo "$AWK -v proc=$cpu -v cpubits=$CPUBITS -f $awkfile ${fpc_sysnr} > $tmp_fpc_sysnr"
  $AWK -v proc=$cpu -v cpubits=$CPUBITS -f $awkfile ${fpc_sysnr} > $tmp_fpc_sysnr
  fpc_sysnr=$tmp_fpc_sysnr
fi
sed -n "s:^\(.*\)*[ \t]*${fpc_syscall_prefix}\\([_a-zA-Z0-9]*\\)[ \t]*=[ \t]*\\(.*\\);\\(.*\\)$:check_c_syscall_number_from_fpc_rtl \2 \"\3\" \"\1 \4\":p" $fpc_sysnr > check_sys_list.sh


sed -n "s:^.*#[[:space:]]*define[[:space:]]*${syscall_prefix}\\([_a-zA-Z0-9]*\\)[[:space:]]*\\([0-9]*\\)\\(.*\\)$:check_c_syscall_number_in_fpc_rtl \1 \"\2\" \"\3\":p" ${syscall_header} > check_sys_list_reverse.sh
 
forward_count=0
forward_ok_count=0
forward_failure_count=0

function check_c_syscall_number_from_fpc_rtl ()
{
  bare_sys=$1
  sys=${syscall_prefix}$bare_sys
  let "value=$2"
  comment="$3"
  if [[ ! ( ( -n "$value" ) && ( $value -ge 0 ) ) ]] ; then
    echo "Computing $2 value"
    let value=$2
  fi
  obsolete=0
  let forward_count++
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
  echo -en "Testing $sys value $value                    \r"
  found=`sed -n "/#[[:space:]]*define[[:space:]]*${sys}[^A-Za-z0-9_]/p" ${syscall_header}`
  val=`sed -n "s:#[[:space:]]*define[[:space:]]*${sys}[^A-Za-z0-9_][^A-Za-z0-9_]*\([0-9]*\).*:\1:p" ${syscall_header}`
  $CC $CC_OPT -DSYS_MACRO=${syscall_prefix}${bare_sys} -o ./test_c_${bare_sys} $c_syscall_source > ./test_${bare_sys}.comp-log 2>&1
  C_COMP_RES=$?
  if [ $C_COMP_RES -eq 0 ] ; then
    CC_value=`./test_c_${bare_sys} `
    if [ "$value" != "$CC_value" ] ; then
      echo "$CC returns $CC_value, while $value is expected"
      let forward_failure_count++
      return
    else
      val=$CC_value
      rm -f ./test_c_${bare_sys}
    fi
    rm -f ./test-${bare_sys}.comp-log
  else
    echo "$CC failed to compile code containing $sys syscall number $value"
    echo "$CC $CC_OPT -DSYS_MACRO=${syscall_prefix}${bare_sys} -o ./test_c_${bare_sys} $c_syscall_source > ./test_${bare_sys}.comp-log 2>&1"
    let forward_failure_count++
    return
  fi

  if [ $verbose -ne 0 ] ; then
    echo Test for $sys found \"${found}\" \"${value}\" \"${val}\"
  fi
  if [ "${val}" == "${value}" ] ; then
    if [ $verbose -ne 0 ] ; then
      echo ${sys} value ${val} is correct
    fi
    let forward_ok_count++
  else
    if [ -z "${val}" ] ; then
      found=`sed -n ".*define[[:space:]].*[^A-Za-z0-9_][[:space:]]${value}$/p" ${syscall_header}`
      if [ -z "${found}" ] ; then
        found=`sed -n "s:\/\* ${value} is compa:/* ${value} is compa:p" ${syscall_header}`
        if [ -n "$found" ] ; then
          obsolete=1
        fi
      fi
    fi
    if [ -z "$found" ] ; then
      found=`grep -n -w $value ${syscall_header}`
    fi
    if [ $obsolete -eq 1 ] ; then
      echo Warning: ${bare_sys} expected ${value}, is obsolete line is \"${found}\"
    else
      echo Problem: ${bare_sys} expected ${value}, line is \"${found}\", val found is \"${val}\"
    fi
    let forward_failure_count++
  fi
}

reverse_count=0
reverse_ok_count=0
reverse_failure_count=0
add_file=./add_missing_syscalls.inc
suggested_addition_count=0
echo "{ Generated by check_rtl_sys.sh script }" > $add_file

function check_c_syscall_number_in_fpc_rtl ()
{
  bare_sys=$1
  sys=${fpc_syscall_prefix}${bare_sys}
  c_sys=${syscall_prefix}${bare_sys}
  value=$2
  if [ -z "$value" ] ; then
    let "value=$3"
    comment="expression $3"
  else
    comment="$3"
  fi
  echo -en "Testing $sys value $value                        \r"
  $CC $CC_OPT -DSYS_MACRO=${c_sys} -o ./test_c_${bare_sys} $c_syscall_source > ./test_${bare_sys}.comp-log 2>&1
  C_COMP_RES=$?
  if [ $C_COMP_RES -eq 0 ] ; then
    rm ./test_${bare_sys}.comp-log
    CC_value=`./test_c_${bare_sys} `
    if [ "$value" != "$CC_value" ] ; then
      echo "For sys=$sys, $CC returns $CC_value, while $value is expected"
      let reverse_failure_count++
      return
    else
      rm -f ./test_c_$bare_sys
    fi
  else
    # if C syscall is not accepted do nothing
    #echo "For sys=$sys, $CC compilation failed"
    #cat ./test_${bare_sys}.comp-log
    # let reverse_failure_count++
    rm -f ./test_c_${bare_sys}
    rm ./test_${bare_sys}.comp-log
    return
  fi

  if [ $verbose -ne 0 ] ; then
    echo "Full comment is \"$comment \""
  fi
  if [ "${comment/*\/\*/}" != "$comment" ] ; then
    comment="${comment/*\/\*/}"
    if [ $verbose -ne 0 ] ; then
      echo "comment is \"$comment \""
    fi
    comment="${comment/\*\/*/}"
    if [ $verbose -ne 0 ] ; then
      echo "comment is \"$comment \""
    fi
    comment=`echo $comment | sed 's:^[[:space:]]*\(.*\)[[:space:]]*$:\1' `
    if [ $verbose -ne 0 ] ; then
      echo "comment is \"$comment \""
    fi
  fi
  if [ $verbose -ne 0 ] ; then
    echo Testing syscall header entry $sys value $value
  fi
  let reverse_count++
  found=`sed -n "/.*${sys}/p" ${fpc_sysnr}`
  val=`sed -n "s:.*${sys}[ \t]*=[ \t]*\([0-9]*\).*:\1:p" ${fpc_sysnr}`
  if [ $verbose -ne 0 ] ; then
    echo Test for $sys found \"${found}\" \"${value}\" \"${val}\"
  fi
  if [ "${val}" == "${value}" ] ; then
    if [ $verbose -ne 0 ] ; then
      echo ${sys} value ${val} is correct
    fi
    let reverse_ok_count++
  else
    if [ -z "${val}" ] ; then
      found=`sed -n "/#[[:space:]]*define.*[^A-Za-z0-9_][[:space:]]*${value}([[:space:]]|$)/p" ${syscall_header}`
      if [ -z "${found}" ] ; then
        found=`sed -n "s:\/\*.*i[[:space:]]${value} is compa: ${value} is compa:p" ${syscall_header}`
      fi
    fi
    echo "Problem: ${bare_sys} expected ${value}, line is \"${found}\", val found is \"${val}\""
    if [ -n "$comment" ] ; then
      echo "    ${fpc_syscall_prefix}${bare_sys} = ${value}; { $comment }" >> $add_file
      echo "Suggest adding:  ${fpc_syscall_prefix}${bare_sys} = ${value}; { $comment }"
    else
      echo "    ${fpc_syscall_prefix}${bare_sys} = ${value};" >> $add_file
      echo "Suggest adding:  ${fpc_syscall_prefix}${bare_sys} = ${value};"
    fi
    let suggested_addition_count++
    let reverse_failure_count++
  fi
}


# Sustitution made to pass from fpc syscall number
# to system define 
set -f

echo "Checking values from \"${fpc_sysnr}\" in C syscall headers"
source ./check_sys_list.sh
echo "Checking if values in C syscall headers are in \"${fpc_sysnr}\""
source ./check_sys_list_reverse.sh
echo "Forward counts: OK=$forward_ok_count, failures=$forward_failure_count, total=$forward_count"
echo "Reverse counts: OK=$reverse_ok_count, failures=$reverse_failure_count, total=$reverse_count"
if [ $suggested_addition_count -gt 0 ] ; then
  echo "Missing $suggested_addition_count syscall numbers in $add_file"
else
 rm $add_file
fi
rm ./check_sys_list.sh ./check_sys_list_reverse.sh ./$awkfile
if [ -f "$tmp_fpc_sysnr" ] ; then
  echo   rm $tmp_fpc_sysnr
fi
