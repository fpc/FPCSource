{ %CPU=i386 }
{ %OPT=-S2cgi -OG2 -gl -al }
program bugloop;

{$mode objfpc}

var
  d: array[0..0] of double;
  i: integer;

begin
  for i := low(d) to high(d) do
    d[i] := i;
  writeln('ok');
end.

