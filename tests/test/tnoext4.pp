{ %norun }
{$mode objfpc}
{x-}

type
 tr = record end;

operator +(const r1,r2: tr) res: tr;
begin
end;

operator:=(const r: tr) res: longint;
begin
end;

var
 r1, r2: tr;
 l:longint;
begin
  l:=r1+r2;
end.