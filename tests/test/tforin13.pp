{mode objfpc}
{$apptype console}
var
  ch: Char;
begin
  for ch in ['a'..'c', '0'..'3', '_'] do 
    Writeln(ch);
end.
