set LONGLOG=1
ppc386 -l >longlog
make alltbf
make tbs0to99
make tbs100to199
make tbs200to299
make tbs300to399
make tbsexec0to99
make tbsexec100to199
make tbsexec200to299
make tbsexec300to399
make allts alltf allto 
make allwebtbf allwebtbs
make allwebtbsexec
make alltsexec
cp log go32v2.log
cp longlong go32v2.longlog
set LONGLOG=1
set FPC=/cvs/bin/ppwin32
make clean
%FPC% -l > longlog
make alltbf
make tbs0to99
make tbs100to199
make tbs200to299
make tbs300to399
make tbsexec0to99
make tbsexec100to199
make tbsexec200to299
make tbsexec300to399
make allts alltf allto 
make allwebtbf allwebtbs
make allwebtbsexec
make alltsexec
cp log win32.log
cp longlong win32.longlog



  
