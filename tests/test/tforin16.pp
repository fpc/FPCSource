{mode objfpc}
{$apptype console}
const S = 'abc';
var ch: Char;
begin
  for ch in S do Writeln(ch);
end.
