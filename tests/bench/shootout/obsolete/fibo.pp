program fibonacci;

{$mode objfpc}

uses SysUtils;

function fib(const N: cardinal): cardinal;
begin
  if N < 2 then fib := 1 else
    fib := fib(N-2) + fib(N-1);
end;

var
  NUM : integer;
  f   : cardinal;

begin
  if ParamCount = 0 then NUM := 1
    else NUM := StrToInt(ParamStr(1));

  if NUM < 1 then NUM := 1;
  f := fib(NUM);
  WriteLn( IntToStr(f) );
end.
