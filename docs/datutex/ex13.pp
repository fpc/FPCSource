Program Example13;

{ This program demonstrates the WeeksInAYear function }

Uses SysUtils,DateUtils;

Var
  Y : Word;

Begin
  For Y:=1992 to 2010 do
    Writeln(Y,' has ',WeeksInAYear(Y),' weeks.');
End.