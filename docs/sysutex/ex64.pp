Program Example64;

{ This program demonstrates the BCDToInt function }

Uses sysutils;

Procedure Testit ( L : longint);
begin
  Writeln (L,' -> ',BCDToInt(L));
end;

Begin
  Testit(10);
  Testit(100);
  Testit(1000);
End.