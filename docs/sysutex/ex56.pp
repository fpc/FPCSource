Program Example56;

{ This program demonstrates the AnsiStrLComp function }

Uses sysutils;

Procedure TestIt (S1,S2 : Pchar; L : longint);

Var R : Longint;

begin
  R:=AnsiStrLComp(S1,S2,L);
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
  Testit('One string','One String',4);
  Testit('One string','1 string',0);
  Testit('One string','One string.',9);
End.