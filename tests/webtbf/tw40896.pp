{ %fail }
{$mode extendedpascal}
type
  fruits = (apple, banana, citrus);
var
  a:fruits;
begin
  Writeln(pred(high(a)));
  ReadLn;
end.
