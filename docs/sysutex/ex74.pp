Program Example74;

{ This program demonstrates the IntToStr function }

Uses sysutils;

Var I : longint;

Begin
  For I:=0 to 31 do
      begin
      Writeln (IntToStr(1 shl I));
      Writeln (IntToStr(15 shl I));
      end;
End.