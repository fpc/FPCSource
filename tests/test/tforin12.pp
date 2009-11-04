{mode objfpc}
{$apptype console}
type T = array [1..3] of Char;
var
  a: array [1..3] of T = ('asd', 'sdf', 'ddf');
  s: T;
begin
  for s in a do 
    Writeln(s);
end.
