{$UNITPATH ../../rtl/linux}
{$INCLUDEPATH ../../rtl/unix;../../rtl/inc;../../rtl/unix;../../rtl/sparc}
PROGRAM SparcTest;
CONST
  s:STRING='Hello World!';
BEGIN
  WriteLn(s);
END.
