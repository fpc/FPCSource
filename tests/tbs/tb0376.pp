{%OPT=-Sew}
{$mode objfpc}

function f: longint;
var
   a: longint absolute result;
begin
   a := 5;
end;

begin
  f;
end.
