Program Example55;

{ This program demonstrates the AnsiStrIComp function }

Uses sysutils;

Procedure TestIt (S1,S2 : Pchar);

Var R : Longint;

begin
  R:=AnsiStrIComp(S1,S2);
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