Program Example57;

{ This program demonstrates the AnsiStrLIComp function }

Uses sysutils;

Procedure TestIt (S1,S2 : Pchar; L : longint);

Var R : Longint;

begin
  R:=AnsiStrLIComp(S1,S2,L);
  Write ('First ',L,' characters of "',S1,'" are ');
  If R<0 then
    write ('less than ')
  else If R=0 then
    Write ('equal to ')
  else
    Write ('larger than ');
  Writeln ('those of "',S2,'"');
end;

Begin
  Testit('One string','One smaller string',255);
  Testit('ONE STRING','one String',4);
  Testit('One string','1 STRING',0);
  Testit('One STRING','one string.',9);
End.