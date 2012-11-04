program Project1;

var
  s: ansistring;
  i : integer;

begin
  s := 'abc';

  i:=1;

  if low(s)<>i then
    halt(1);

  if high(s)<>3 then
    halt(1);

  i:=0;

{$ZEROBASEDSTRINGS ON}
  if low(s)<>i then
    halt(1);

  if high(s)<>2 then
    halt(1);

  if s[0]<>'a' then
    halt(1);
{$ZEROBASEDSTRINGS OFF}
  writeln('ok');
end.
