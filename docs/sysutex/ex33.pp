Program Example33;

{ This program demonstrates the ExpandFileName function }

Uses sysutils;

Procedure Testit (F : String);

begin
  Writeln (F,' expands to : ',ExpandFileName(F));
end;

Begin
  Testit('ex33.pp');
  Testit(ParamStr(0));
  Testit('/pp/bin/win32/ppc386');
  Testit('\pp\bin\win32\ppc386');
  Testit('.');
End.