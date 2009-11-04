{$mode objfpc}
{$apptype console}
type T = (a1, b1=5);
var
  ch: T;
begin
  for ch in T do Writeln(ch);
end.
