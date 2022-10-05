{ %opt=-Sc }
{$mode objfpc}
{$modeswitch arrayoperators}

program test;

var
  a: array of integer;
begin
  a := nil;
  a += []; // error: Internal error 200305091
  a := a+[]; // error: Internal error 200305091
  a := []+a; // error: Internal error 200305091
  if length(a)<>0 then
    halt(1);
end.
