{$UNITPATH ../../rtl/linux}
{$INCLUDEPATH ../../rtl/unix;../../rtl/inc;../../rtl/unix;../../rtl/sparc}
PROGRAM SparcTest;
VAR
  i:Integer;
BEGIN
  FOR i:=0 TO MaxInt DO
    WriteLn(i);
END.
