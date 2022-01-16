{$ifndef FPUNONE}
uses
  math;

var
  a,b,c : float;
begin
  a:=7.7;
  b:=1.1;
  c:=a mod b;
  if not(SameValue(c,0.0)) then
    begin
      writeln(c);
      halt(1);
    end;
  writeln('ok');
{$else FPUNONE}
begin
{$endif FPUNONE}
end.


