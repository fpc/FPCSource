program takfp;

{$mode objfpc}

uses SysUtils;

function Tak(const x, y, z: double): double;
begin
  if y >= x then Tak:=z else
    Tak:=Tak(Tak(x-1,y,z), Tak(y-1,z,x), Tak(z-1,x,y));
end;

var n: integer;
begin
  if paramcount<1 then n:=1
    else n:=StrToInt(paramstr(1));
  writeln(Tak(n*3, n*2, n*1):0:1);
end.
