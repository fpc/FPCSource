{$UNITPATH ../../rtl/linux}
{$INCLUDEPATH ../../rtl/unix;../../rtl/inc;../../rtl/unix;../../rtl/sparc}
PROGRAM SparcTest;
VAR
  i:Integer;
BEGIN
  IF(i>0)
  THEN
    WriteLn(i);
END.
