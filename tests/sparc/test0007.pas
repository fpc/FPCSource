{$UNITPATH ../../rtl/linux}
{$INCLUDEPATH ../../rtl/unix;../../rtl/inc;../../rtl/unix;../../rtl/sparc}
PROGRAM SparcTest;
VAR
  c:Char;
BEGIN
  IF(c='0')
  THEN
    WriteLn(c);
END.
