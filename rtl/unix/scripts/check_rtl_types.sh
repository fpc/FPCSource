#!/usr/bin/env bash
filename="$1"
shift

verbose=0
i=1
while [ $i -le $# ] ; do
  arg="${!i}"
  echo "Handling arg $i, \"$arg\""
  if [ "${arg//=}" != "$arg" ] ; then
    echo "Evaluating \"$arg\""
    arg2="${arg/=*/}=\"${arg/*=/}\""
    eval "$arg2"
  elif [ "$arg" == "-v" ] ; then
    verbose=1
  else
    FPC_OPTS="$FPC_OPTS $arg"
  fi
  let i++
done

if [ ! -f "$filename" ] ; then
  echo "Usage: $0 file.h2paschk"
  exit
fi

filebase=${filename/.*/}

filebaseonly=`basename $filebase`

filedir=`dirname $filename`
if [ -z "$filedir" ] ; then
  filedir=.
fi

if [ -z "$MAKE" ] ; then
  MAKE=`which gmake`
fi

if [ -z "$MAKE" ] ; then
  MAKE=`which make`
fi

if [ -z "$FPC" ] ; then
  FPC=fpc
  default_fpc=1
else
  default_fpc=0
fi

if [ -z "$CC" ] ; then
  CC=`which gcc`
fi

if [ -z "$CC" ] ; then
  CC=`which cc`
fi

if [ -f "{$filebase}.c" ] ; then
  rm -f ${filebase}.c
fi
if [ -f "{$filebase}.pas" ] ; then
  rm -f ${filebase}.pas
fi

function check_one ()
{
VERSION=$1

echo "Calling h2paschk $filename"
h2paschk $filename
res=$?
if [ $res -ne 0 ] ; then
  exit
fi

TMP_DIR=tmp_$VERSION
if [ -d $TMP_DIR ] ; then
  rm -Rf $TMP_DIR
fi
mkdir $TMP_DIR

mv ${filebase}.c ${filebase}.pas $TMP_DIR
cd $TMP_DIR
echo "Calling $CC $CC_OPT -o ${filebase}_${VERSION}_c ${filebase}.c"
$CC $CC_OPT -o ${filebase}_${VERSION}_c ${filebase}.c > ${filebase}_${VERSION}_c.comp.log 2>&1
res=$?
if [ $res -ne 0 ] ; then
  echo "$CC call failed in $VERSION, res=$res"
  cat ${filebase}${VERSION}_c.comp.log
  exit
fi

./${filebase}_${VERSION}_c > ${filebase}_${VERSION}_c.out
res=$?
if [ $res -ne 0 ] ; then
  echo "./${filebase}_${VERSION}_c failed in $VERSION, res=$res"
  exit
fi

echo "Calling $MAKE -C .. all OPT=\"-n -gwl $FPC_OPTS\" FPC=$FPC"
$MAKE -C .. all OPT="-n -gwl $FPC_OPTS" FPC=$FPC > ${filebase}${VERSION}_make_all.log 2>&1
res=$?
if [ $res -ne 0 ] ; then
  echo "$MAKE call failed in $VERSION, res=$res"
  cat ${filebase}${VERSION}_make_all.log
  exit
fi

OS_TARGET=`$FPC $FPC_OPTS  -iTO`
CPU_TARGET=`$FPC $FPC_OPTS -iTP`
echo "Calling $MAKE -C .. ${TMP_DIR}/${filebaseonly} FPC=$FPC OPT=\"-n -gwl $FPC_OPTS\" -Fu../units/$CPU_TARGET-$OS_TARGET"
$MAKE -C .. ${TMP_DIR}/${filebaseonly} FPC=$FPC OPT="-n -gwl $FPC_OPTS -Fu../units/$CPU_TARGET-$OS_TARGET" > ${filebase}_${VERSION}_pas.comp.log 2>&1
res=$?
if [ $res -ne 0 ] ; then
  echo "$FPC call failed in $VERSION, res=$res"
  cat ${filebase}_${VERSION}_pas.comp.log
  exit
fi
mv -f ../${filebase} ./${filebase}_${VERSION}_pas

./${filebase}_${VERSION}_pas > ${filebase}_${VERSION}_pas.out
res=$?
if [ $res -ne 0 ] ; then
  echo "./${filebase}${VERSION} call failed in $VERSION, res=$res"
  exit
fi

diff ${filebase}_${VERSION}_c.out ${filebase}_${VERSION}_pas.out > ${filebase}_${VERSION}.diffs
res=$?
if [ $res -eq 0 ] ; then
  echo "No difference found!"
else
  echo "Diffs for ${VERSION} are:"
  echo "< C      results"
  echo "> Pascal results"
  cat ${filebase}_${VERSION}.diffs
fi
# Clean up
if [ $verbose -eq 0 ] ; then
  rm -f ${filebase}_${VERSION}_c
  rm -f ${filebase}_${VERSION}_pas
  rm -f ${filebase}_${VERSION}_c.out
  rm -f ${filebase}_pas${VERSION}.out
  rm -f ${filebase}_${VERSION}_c.comp.log
  rm -f ${filebase}_${VERSION}_pas.comp.log
  rm -f ${filebase}_${VERSION}_make_all.log
  rm -f ${filebase}.c
  rm -f ${filebase}.pas
fi
cd ..
}

function check_64 ()
{
  if [ "$FPC64" == "ppca64" ] ; then
    CC_OPT="-Wall"
  else
    CC_OPT="-m64 -Wall"
  fi
  if [ $default_fpc -eq 1 ] ; then  
    FPC=$FPC64
    if [ "$CPU_SOURCE" != "$CPU_TARGET" ] ; then
      FPC_OPTS="$FPC_OPTS -XP${CPU_TARGET}-${OS_SOURCE}-"
    fi
  fi
  check_one 64bit
}

function check_32 ()
{
  if [ "$CPU_SOURCE" == "aarch64" ] ; then
    CC=arm-linux-gnueabihf-gcc-4.8
    export BINUTILSPREFIX=arm-linux-
  fi
  if [ "$FPC32" == "ppcarm" ] ; then
    CC_OPT="-march=armv7-a -Wall"
  else
    CC_OPT="-m32 -Wall"
  fi

  FPC=$FPC32  
  if [ "$CPU_SOURCE" != "$CPU_TARGET" ] ; then
    FPC_OPTS="$FPC_OPTS -XP${CPU_TARGET}-${OS_SOURCE}-"
  fi
  check_one 32bit
}

function check_gen32 ()
{
  CC_OPT="-m32 -Wall"

  check_one gen32bit
}

OS_SOURCE=`$FPC $FPC_OPTS  -iSO`
CPU_SOURCE=`$FPC $FPC_OPTS -iSP`
CPU_TARGET=`$FPC $FPC_OPTS -iTP`
case $CPU_SOURCE in
  aarch64) FPC32=ppcarm; FPC64=ppca64;;
  arm) FPC32=ppcarm; FPC64=;;
  x86_64) FPC32=ppc386; FPC64=ppcx64;;
  i386) FPC32=ppc386; FPC64=;;
  powerpc64) FPC32=ppcppc; FPC64=ppcppc64;;
  powerpc) FPC32=ppcppc; FPC64=;;
  riscv64) FPC32=ppcrv32; FPC64=ppcrv64;;
  riscv32) FPC32=ppcrv32; FPC64=;;
  sparc64) FPC32=ppcsparc; FPC64=ppcsparc64;;
  sparc) FPC32=ppcsparc; FPC64=;;
  m68k) FPC32=ppc68k; FPC64=;;
  mips) FPC32=ppcmips; FPC64=;;
  mipsel) FPC32=ppcmipsel; FPC64=;;
esac

# No i386<->x86_64 cross-compilation on OpeenBSD
if [ "$OS_SOURCE" == "openbsd" ] ; then
  if [ "$CPU_SOURCE" == "i386" ] ; then
    FPC64=
  else
    FPC32=
  fi
fi


if [ $default_fpc -eq 1 ] ; then
  if [ -n "$FPC64" ] ; then
    check_64
  fi

  if [ -n "$FPC32" ] ; then
    check_32
  fi
else
  if [ "${FPC}" == "$FPC64" ] ; then
    check_64
  elif [ "${FPC}" == "$FPC32" ] ; then
    check_32
  else
    echo "Unrecognized FPC=\"$FPC\""
  fi
fi



