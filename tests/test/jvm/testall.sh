#!/bin/bash

set -ex

ppcjvm -O2 -g unsupported
ppcjvm -O2 -g testintf
ppcjvm -O2 -g nested
ppcjvm -O2 -g test
javac -encoding utf-8 -cp ../../../rtl/units/jvm-java:. JavaClass.java
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. JavaClass
ppcjvm -O2 -g sort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. sort
ppcjvm -O2 -g classmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. classmeth
ppcjvm -O2 -g classlist
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. classlist
ppcjvm -O2 -g testansi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. testansi
ppcjvm -O2 -g tcnvstr1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tcnvstr1
ppcjvm -O2 -g tcnvstr3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tcnvstr3
ppcjvm -O2 -g testshort
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. testshort
ppcjvm -O2 -g tarray2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tarray2
ppcjvm -O2 -g tarray3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tarray3
ppcjvm -O2 -g tnestproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tnestproc
ppcjvm -O2 -g outpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. outpara
ppcjvm -O2 -g tbytearrres
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tbytearrres
ppcjvm -O2 -g forw
ppcjvm -O2 -g tbyte
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tbyte
rm -f uenum.ppu
ppcjvm -O2 -g tenum
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tenum
ppcjvm -O2 -g tprop
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tprop
ppcjvm -O2 -g tprop2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tprop2
ppcjvm -O2 -g tclassproptest
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tclassproptest
ppcjvm -O2 -g tset3 -dproc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset3
ppcjvm -O2 -g tset3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset3
ppcjvm -O2 -g taddset
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. taddset
ppcjvm -O2 -g taddsetint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. taddsetint
ppcjvm -O2 -g tformalpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tformalpara
ppcjvm -O2 -g tvarpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tvarpara
ppcjvm -O2 -g tpvar
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvar
ppcjvm -O2 -g tpvardelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvardelphi
ppcjvm -O2 -g tpvarglobal
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvarglobal
ppcjvm -O2 -g tpvarglobaldelphi
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tpvarglobaldelphi
ppcjvm -O2 -g tvirtclmeth
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tvirtclmeth
ppcjvm -O2 -g tdynarrec
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tdynarrec
ppcjvm -O2 -g tconst
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tconst
ppcjvm -O2 -g twith
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. twith
ppcjvm -O2 -g tint
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tint
ppcjvm -O2 -g ttrig
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. ttrig
ppcjvm -O2 -g ttrunc
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. ttrunc
ppcjvm -O2 -g tset1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tset1
ppcjvm -O2 -g tabs
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tabs
ppcjvm -O2 -g tintstr
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tintstr
ppcjvm -O2 -g trange1
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange1
ppcjvm -O2 -g trange2
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange2
ppcjvm -O2 -g trange3
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. trange3
ppcjvm -O2 -g tdefpara
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. tdefpara
ppcjvm -O2 -g getbit
java -Dfile.encoding=UTF-8 -cp ../../../rtl/units/jvm-java:. getbit
