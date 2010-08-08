{$mode objfpc}

type
  tr1 = record
    s: single;
  end;

  tr2 = record
    case byte of
      1: (s: single);
  end;

function f1(r1:tr1): tr1;
var
  s: single;
begin
  s:=r1.s;
  result.s:=s;
end;

function f2(r2:tr2): tr2;
var
  s: single;
begin
  s:=r2.s;
  result.s:=s;
end;

procedure test;
var
  r1,r1a: tr1;
  r2,r2a: tr2;
begin
  r1.s:=1.0;
  r2.s:=2.0;
  r1a:=f1(r1);
  r2a:=f2(r2);
  if r1a.s<>1.0 then
    halt(1);
  if r2a.s<>2.0 then
    halt(1);
end;

begin
  test;
end.
