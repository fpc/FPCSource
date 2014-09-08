{ %norun }
{$inline on}
procedure p(const s : ansistring);inline;
begin
  writeln(s[1]);
end;

begin
  p('');
end.
