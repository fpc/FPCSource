program testinline;

{$mode objfpc}{$H+}

type
  enumtest = (e1, e2);

function Test: Boolean; inline;
var
  e: enumtest;
  s: String;
begin
  e := e1;
  WriteStr(s, e);
  result:=s='e1';
end;

procedure TestProc;
begin
  if not Test then
    halt(1);
end;

begin
  TestProc;
end.

