#!/bin/bash

set -ex

if [ $# -eq 0 ]; then
  PPC=ppcppc
else
  PPC="$1"
fi

$PPC -O2 -g unsupported
$PPC -O2 -g testintf
$PPC -O2 -g nested
$PPC -O2 -g test
javac -encoding utf-8 -cp ../../../rtl/units/jvm-java:. JavaClass.java
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. JavaClass
$PPC -O2 -g sort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. sort
$PPC -O2 -g classmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. classmeth
$PPC -O2 -g classlist
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. classlist
$PPC -O2 -g testansi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. testansi
$PPC -O2 -g tcnvstr1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tcnvstr1
$PPC -O2 -g tcnvstr3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tcnvstr3
$PPC -O2 -g testshort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. testshort
$PPC -O2 -g tarray2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tarray2
$PPC -O2 -g tarray3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tarray3
$PPC -O2 -g tnestproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tnestproc
$PPC -O2 -g outpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. outpara
$PPC -O2 -g tbytearrres
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tbytearrres
$PPC -O2 -g forw
$PPC -O2 -g tbyte
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tbyte
rm -f uenum.ppu
$PPC -O2 -g tenum
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tenum
$PPC -O2 -g tprop
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tprop
$PPC -O2 -g tprop2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tprop2
$PPC -O2 -g tclassproptest
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tclassproptest
$PPC -O2 -g tset3 -dproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset3
$PPC -O2 -g tset3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset3
$PPC -O2 -g taddset
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. taddset
$PPC -O2 -g taddsetint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. taddsetint
$PPC -O2 -g tformalpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tformalpara
$PPC -O2 -g tvarpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tvarpara
$PPC -O2 -g tpvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvar
$PPC -O2 -g tpvardelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvardelphi
$PPC -O2 -g tpvarglobal
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvarglobal
$PPC -O2 -g tpvarglobaldelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvarglobaldelphi
$PPC -O2 -g tvirtclmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tvirtclmeth
$PPC -O2 -g tdynarrec
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tdynarrec
$PPC -O2 -g tconst
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tconst
$PPC -O2 -g twith
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. twith
$PPC -O2 -g tint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tint
$PPC -O2 -g ttrig
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. ttrig
$PPC -O2 -g ttrunc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. ttrunc
$PPC -O2 -g tset1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset1
$PPC -O2 -g tabs
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tabs
$PPC -O2 -g tintstr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tintstr
$PPC -O2 -g trange1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange1
$PPC -O2 -g trange2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange2
$PPC -O2 -g trange3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange3
$PPC -O2 -g tdefpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tdefpara
$PPC -O2 -g getbit
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. getbit
$PPC -O2 -g tthreadvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tthreadvar
$PPC -O2 -g tstring1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tstring1
$PPC -O2 -g tstrreal1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tstrreal1
$PPC -O2 -g tstrreal2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tstrreal2
$PPC -O2 -g -B tval
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tval
$PPC -O2 -g -B tval5
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tval5
$PPC -O2 -g -B tstring9
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tstring9
$PPC -O2 -g -B tstr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tstr
$PPC -O2 -g -B tw20212 
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tw20212
