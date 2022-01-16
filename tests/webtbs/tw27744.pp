{ %opt=-Oodfa -Sew }
{$mode objfpc}

type
  tarr = array of longint;
var
  a: array of longint;

function IndexOfValue(const a: tarr; l: longint): longint;
begin
  for result:=low(a) to high(a) do
    if a[result]=l then
      exit;
  result:=-1;
end;

begin
end.

