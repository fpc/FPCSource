{ %fail }

const
  c1 = high(int64)-1;
  { Overflow }
  c2 = c1*2;
begin
  writeln(c2);
end.

