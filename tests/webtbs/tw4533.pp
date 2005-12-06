{$mode objfpc}
{$openstrings+}
procedure t(out s: shortstring);
begin
  writeln(high(s));
  if high(s) <> 4 then
    halt(1);
end;

var
  s: string[4];
begin
  t(s);
end.

