program example72;

{ Program to demonstrate the NanoSleep function. }

uses oldlinux;

Var
  Req,Rem : TimeSpec;
  Res : Longint;

begin
  With Req do
    begin
    tv_sec:=10;
    tv_nsec:=100;
    end;
  Write('NanoSleep returned : ');
  Flush(Output);
  Res:=(NanoSleep(Req,rem));
  Writeln(res);
  If (res<>0) then
    With rem do
      begin
      Writeln('Remaining seconds     : ',tv_sec);
      Writeln('Remaining nanoseconds : ',tv_nsec);
      end;
end.