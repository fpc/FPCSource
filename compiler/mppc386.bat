ppc386 -OG2p2 -al -Ch8000000 -dI386 -dGDB -a -Sg pp.pas %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 0 goto success
goto failed
:success
copy pp.exe ppc386.exe
:failed
