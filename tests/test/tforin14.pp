{mode objfpc}
{$apptype console}
type
  T = array [1..3] of Integer;


procedure P(a: array of T);
var
  r: T;
  i: Integer;
begin
  for r in a do 
  begin
    for i in r do Write(i, ' ');
    Writeln;
  end;
end;

const
  r1: T = (1,2,9);
  r2: T = (3,4,5);
begin
  P([r1, r2]);
end.
