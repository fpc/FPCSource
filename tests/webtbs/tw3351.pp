{ %opt=-Sc }

{ Source provided for Free Pascal Bug Report 3351 }
{ Submitted by "Maciej &#379;enczykowski" on  2004-10-10 }
{ e-mail: maze@cela.pl }

type
  PInteger = ^Integer;
var
  i : integer;
  t : array [0..1] of integer;

function f : PInteger;
  begin
    f := @t[i];
    Inc(i);
    Write('.');
  end;

begin
  i:=0;
  f^ += 1;
  writeln(t[0], ' ', t[1]);
  if i<>1 then
    halt(1);
end.
