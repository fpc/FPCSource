program example73;

{ Program to demonstrate the FpSleep function. }

uses BaseUnix;

Var
  Res : Longint;

begin
  Write('Sleep returned : ');
  Flush(Output);
  Res:=(fpSleep(10));
  Writeln(res);
  If (res<>0) then
    Writeln('Remaining seconds     : ',res);
end.