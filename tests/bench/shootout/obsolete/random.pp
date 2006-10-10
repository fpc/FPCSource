program random;

{$mode objfpc}

uses SysUtils;

const IM = 139968;
      IA =   3877;
      IC =  29573;

var  LAST, NUM, i: longint;
     value: double;
     
function gen_random(const n: integer): double; inline;
begin
  LAST := (LAST * IA + IC) mod IM;
  gen_random := n * LAST / IM;
end;

begin
  if ParamCount = 0 then NUM := 1
    else NUM := StrToInt(ParamStr(1));
  if NUM < 1 then NUM := 1;
  LAST := 42;
  for i:= 1 to NUM do
    value:=gen_random(100);
  WriteLn(value:10:9);
end.
