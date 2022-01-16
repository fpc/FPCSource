{$mode objfpc}

program test;

type
  TVec2 = record
    x, y: single;
  end;

function ViewToWorld (x, y: single): TVec2; overload;
begin
  result.x := x;
  result.y := y;
end;

function ViewToWorld (pt: TVec2): TVec2; overload; inline;
begin
  result := ViewToWorld(pt.x, pt.y);
end;

var
  pt: TVec2;
begin
  pt.x:=1.0;
  pt.y:=2.0;
  // ERROR: Internal error 2009112601
  pt := ViewToWorld(pt);
  if pt.x<>1.0 then
    halt(1);
  if pt.y<>2.0 then
    halt(2);
end.
