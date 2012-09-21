{ %fail }

program test;

{$mode objfpc}
{$h+}

var
  s: string;

begin
  s[1..3] := '123';
end.
