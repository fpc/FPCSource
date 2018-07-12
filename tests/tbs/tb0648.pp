{$excessprecision on}
const
  d1: double = 1.0/3.0;
  d2: double = 1/3;
  x1: extended = 1.0/3.0;
  x2: extended = 1/3;
  s1: single   = 1.0/3.0;
  s2: single   = 1/3;
begin
  writeln(s1:30:10,  s2:30:10);
  if s1<>s2 then
    halt(1);
  writeln(d1:30:16,  d2:30:16);
  if d1<>d2 then
    halt(1);
{$ifdef FPUX87}
  writeln(x1:30:24,  x2:30:24);
  if x1<>x2 then
    halt(1);
{$endif FPUX87}
  writeln('ok');
end.
