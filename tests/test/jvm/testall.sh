#!/bin/bash

set -ex

RTLDIR=jvm-java

if [ $# -eq 0 ]; then
  PPC=ppcjvm
else
  PPC="$1"
  if [ $# -eq 2 ]; then
    RTLDIR="$2"
  fi
fi

rm -rf org

$PPC -O2 -g unsupported
$PPC -O2 -g testintf
$PPC -O2 -g nested
$PPC -O2 -g test
javac -encoding utf-8 -cp ../../../rtl/units/$RTLDIR:. JavaClass.java
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. JavaClass
$PPC -O2 -g sort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. sort
$PPC -O2 -g classmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. classmeth
$PPC -O2 -g classlist
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. classlist
$PPC -O2 -g testansi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. testansi
$PPC -O2 -g tcnvstr1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tcnvstr1
$PPC -O2 -g tcnvstr3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tcnvstr3
$PPC -O2 -g testshort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. testshort
$PPC -O2 -g tarray2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tarray2
$PPC -O2 -g tarray3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tarray3
$PPC -O2 -g tnestproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tnestproc
$PPC -O2 -g outpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. outpara
$PPC -O2 -g tbytearrres
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tbytearrres
$PPC -O2 -g forw
$PPC -O2 -g tbyte
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tbyte
rm -f uenum.ppu
$PPC -O2 -g -CTenumfieldinit tenum
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tenum
$PPC -O2 -g tenum2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tenum2
$PPC -O2 -g tprop
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop
$PPC -O2 -g tprop2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop2
$PPC -O2 -g tclassproptest
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tclassproptest
$PPC -O2 -g tset3 -dproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tset3
$PPC -O2 -g tset3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tset3
$PPC -O2 -g taddset
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. taddset
$PPC -O2 -g taddsetint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. taddsetint
$PPC -O2 -g tformalpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tformalpara
$PPC -O2 -g tvarpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tvarpara
$PPC -O2 -g tpvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tpvar
$PPC -O2 -g tpvardelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tpvardelphi
$PPC -O2 -g tpvarglobal
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tpvarglobal
$PPC -O2 -g tpvarglobaldelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tpvarglobaldelphi
$PPC -O2 -g tvirtclmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tvirtclmeth
$PPC -O2 -g tdynarrec
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tdynarrec
$PPC -O2 -g tconst
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tconst
$PPC -O2 -g twith
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. twith
$PPC -O2 -g tint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tint
$PPC -O2 -g ttrig
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. ttrig
$PPC -O2 -g ttrunc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. ttrunc
$PPC -O2 -g tset1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tset1
$PPC -O2 -g tabs
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tabs
$PPC -O2 -g tintstr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tintstr
$PPC -O2 -g trange1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. trange1
$PPC -O2 -g trange2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. trange2
$PPC -O2 -g trange3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. trange3
$PPC -O2 -g tdefpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tdefpara
$PPC -O2 -g getbit
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. getbit
$PPC -O2 -g tthreadvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tthreadvar
$PPC -O2 -g tstring1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tstring1
$PPC -O2 -g tstrreal1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tstrreal1
$PPC -O2 -g tstrreal2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tstrreal2
$PPC -O2 -g -B tval
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tval
$PPC -O2 -g -B tval5
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tval5
$PPC -O2 -g -B tstring9
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tstring9
$PPC -O2 -g -B tstr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tstr
$PPC -O2 -g -B tw20212 
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tw20212
$PPC -O2 -g -B tdynarrnil
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tdynarrnil
$PPC -O2 -g -B tnestdynarr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tnestdynarr
$PPC -O2 -g -B topovl
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. topovl
$PPC -O2 -g -B -Sa tassert
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tassert
$PPC -O2 -g -B -Sa taddbool
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. TAddBool
$PPC -O2 -g -B -Sa tsetansistr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tsetansistr
$PPC -O2 -g -B -Sa tw22807
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tw22807
$PPC -O2 -g -B -Sa ttincdec.pp
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. ttincdec
$PPC -O2 -g -B -CTautogetterprefix=Get tprop3
$PPC -O2 -g -B -CTautogetterprefix=Get tprop4
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop4
$PPC -O2 -g -B -Sa tw24089
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tw24089
$PPC -O2 -g -B -Sa -CTautosetterprefix=Set ujsetter
javac -encoding utf-8 -cp ../../../rtl/units/$RTLDIR:. tjsetter.java
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tjsetter
$PPC -O2 -g -B -Sa tlowercaseproc
javac -encoding utf-8 -cp ../../../rtl/units/$RTLDIR:. tjavalowercaseproc.java
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tjavalowercaseproc
$PPC -O2 -g -B -Sa -CTinitlocals tinitvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. org.freepascal.test.tinitvar.tinitvar
$PPC -O2 -g -B -Sa tsmallintarr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tsmallintarr
set +e
$PPC -O2 -g -B -Sa toverload
if [ $? -eq 0 ]; then
  echo " ** Should have failed compilation"
else
  echo " ** Compilation failed as expected"
fi
$PPC -O2 -g -B -Sa toverload2
if [ $? -eq 0 ]; then
  echo " ** Should have failed compilation"
else
  echo " ** Compilation failed as expected"
fi
set -e
$PPC -O2 -g -B -Sa tptrdynarr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tptrdynarr
$PPC -O2 -g -B -Sa tprop5a
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop5a
$PPC -O2 -g -B -Sa tprop5a -CTautosetterprefix=Set -CTautogetterprefix=Get
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop5a
$PPC -O2 -g -B -Sa tprop6a
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop6a
$PPC -O2 -g -B -Sa tprop6a -CTautosetterprefix=Set -CTautogetterprefix=Get
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tprop6a
$PPC -O2 -g -B -Sa tsetstring
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/$RTLDIR:. tsetstring
