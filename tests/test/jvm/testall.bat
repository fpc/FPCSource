@echo off
DEL /S /Q org > NUL 2>&1

..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g unsupported || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g testintf || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g nested || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g test || EXIT /b 1
javac -encoding utf-8 -cp ..\..\..\rtl\units\jvm-java;. JavaClass.java || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. JavaClass || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g sort || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. sort || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g classmeth || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. classmeth || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g classlist || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. classlist || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g testansi || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. testansi || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tcnvstr1 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tcnvstr1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tcnvstr3 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tcnvstr3 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g testshort || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. testshort || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tarray2 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tarray2 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tarray3 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tarray3 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tnestproc || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tnestproc || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g outpara || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. outpara || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tbytearrres || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tbytearrres || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g forw || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tbyte || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tbyte || EXIT /b 1
DEL /Q uenum.ppu > NUL 2>&1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -CTenumfieldinit tenum || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tenum || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tenum2 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tenum2 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tprop || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tprop2 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop2 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tclassproptest || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tclassproptest || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tset3 -dproc || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tset3 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tset3 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tset3 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g taddset || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. taddset || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g taddsetint || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. taddsetint || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tformalpara || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tformalpara || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tvarpara || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tvarpara || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tpvar || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tpvar || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tpvardelphi || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tpvardelphi || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tpvarglobal || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tpvarglobal || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tpvarglobaldelphi || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tpvarglobaldelphi || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tvirtclmeth || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tvirtclmeth || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tdynarrec || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tdynarrec || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tconst || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tconst || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g twith || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. twith || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tint || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tint || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g ttrig || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. ttrig || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g ttrunc || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. ttrunc || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tset1 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tset1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tabs || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tabs || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tintstr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tintstr || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g trange1 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. trange1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g trange2 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. trange2 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g trange3 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. trange3 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tdefpara || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tdefpara || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g getbit || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. getbit || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tthreadvar || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tthreadvar || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tstring1 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstring1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tstrreal1 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstrreal1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g tstrreal2 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstrreal2 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tval || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tval || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tval5 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tval5 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tstring9 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstring9 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tstr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstr || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tw20212 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tw20212 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tdynarrnil || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tdynarrnil || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B tnestdynarr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tnestdynarr || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B topovl || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. topovl || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tassert || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tassert || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa taddbool || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. TAddBool || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tsetansistr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tsetansistr || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tw22807 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tw22807 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa ttincdec.pp || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. ttincdec || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTautogetterprefix=Get tprop3.pp || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTautogetterprefix=Get tprop4.pp || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop4 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tw24089 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tw24089 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa -CTautosetterprefix=Set ujsetter || EXIT /b 1
javac -encoding utf-8 -cp ..\..\..\rtl\units\jvm-java;. tjsetter.java || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tjsetter || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tlowercaseproc || EXIT /b 1
javac -encoding utf-8 -cp ..\..\..\rtl\units\jvm-java;. tjavalowercaseproc.java || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tjavalowercaseproc || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tinitvar || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. org.freepascal.test.tinitvar.tinitvar || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tsmallintarr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tsmallintarr || EXIT /b 1

' Negative tests - successful compilation (ERRORLEVEL 0) = failure

..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -vh toverload && EXIT /b 1
echo " ** Compilation failed as expected"
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B toverload2 && EXIT /b 1
echo " ** Compilation failed as expected"

..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tptrdynarr || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tptrdynarr || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tprop5a || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop5a || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tprop5a -CTautosetterprefix=Set -CTautogetterprefix=Get || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop5a || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tprop6a || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop6a || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tprop6a -CTautosetterprefix=Set -CTautogetterprefix=Get || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprop6a || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tsetstring || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tsetstring || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -Sa tnestcallpass1 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tformalclass || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tformalclass || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tprocvaranon || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tprocvaranon || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tw29585 || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tw29585 || EXIT /b 1
..\..\..\compiler\ppcjvm -Fu..\..\..\rtl\units\jvm-java -O2 -g -B -CTinitlocals tstring || EXIT /b 1
java -Dfile.encoding=UTF-8 -cp ..\..\..\rtl\units\jvm-java;. tstring || EXIT /b 1


ECHO ----------------
ECHO JVM tests passed
ECHO ----------------