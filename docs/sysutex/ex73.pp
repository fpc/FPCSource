Program Example73;

{ This program demonstrates the IntToHex function }

Uses sysutils;

Var I : longint;

Begin
  For I:=0 to 31 do
      begin
      Writeln (IntToHex(1 shl I,8));
      Writeln (IntToHex(15 shl I,8))
      end;
End.