{$UNITPATH ../../rtl/linux}
{$INCLUDEPATH ../../rtl/unix;../../rtl/inc;../../rtl/unix;../../rtl/sparc}
PROGRAM SparcTest;
VAR
  c:Char;
BEGIN
  CASE c OF
    '0':
      WriteLn(0);
    '1':
      WriteLn(1);
    '2':
      WriteLn('2');
  END;
END.
