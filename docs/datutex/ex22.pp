Program Example22;

{ This program demonstrates the PreviousDayOfWeek function }

Uses SysUtils,DateUtils;

Var
  D : Word;

Begin
  For D:=1 to 7 do
    Writeln('Previous day of ',D,' is : ',PreviousDayOfWeek(D));
End.