Program Example66;

{ This program demonstrates the CompareText function }
{$H+}

Uses sysutils;

Procedure TestIt (S1,S2 : String);

Var R : Longint;

begin
  R:=CompareText(S1,S2);
  Write ('"',S1,'" is ');
  If R<0 then
    write ('less than ')
  else If R=0 then
    Write ('equal to ')
  else
    Write ('larger than ');
  Writeln ('"',S2,'"');
end;

Begin
  Testit('One string','One smaller string');
  Testit('One string','one string');
  Testit('One string','One string');
  Testit('One string','One tall string');
End.