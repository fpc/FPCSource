program ackerman;

{$mode objfpc}

uses SysUtils;

function Ack(const M, N : integer) : integer;
begin
  if M = 0 then Ack := N+1
    else if N = 0 then Ack := Ack(M-1, 1)
      else Ack := Ack(M-1, Ack(M, N-1));
end;

var NUM, a: integer;
begin
  if ParamCount = 0 then NUM := 1
    else NUM := StrToInt(ParamStr(1));
  if NUM < 1 then NUM := 1;

  a := Ack(3, NUM);
  WriteLn( 'Ack(3,' + IntToStr(NUM) + '): ' + IntToStr(a) );
end.
